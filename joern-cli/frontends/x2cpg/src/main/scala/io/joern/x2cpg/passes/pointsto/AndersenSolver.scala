package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Inclusion-based (Andersen) pointer analysis with k=1 object sensitivity.
  *
  * Every method is first instantiated under a default context. Virtual calls trigger re-instantiation of the callee
  * under a fresh context keyed by the receiver's allocation site (k=1 object sensitivity). Static calls pass the
  * caller's context through. Field slots are context-insensitive.
  *
  * Pointer variables are packed as `"$ctx|$var"` strings; field slots (prefix `F:`) are always packed under the
  * default context. Fixed point is reached because all operations are monotone (sets only grow, instantiation is
  * memoised).
  */
final class AndersenSolver(
  cpg: Cpg,
  allocTable: AllocationSiteTable,
  constraintsByMethod: Map[String, IndexedSeq[Constraint]]
) {

  import Constraint.*

  /** Default context key, used for baseline instantiation and context-insensitive field slots. */
  private val DEFAULT_CTX: Int = -1

  /** Pack a pointer variable under a context. Field slots are always packed under [[DEFAULT_CTX]]. */
  @inline private def k(ctx: Int, v: String): String =
    if (v.startsWith("F:")) s"$DEFAULT_CTX|$v"
    else s"$ctx|$v"

  /** Points-to map: packed variable key → set of allocation-site indices. */
  private val pt = mutable.HashMap.empty[String, PointsToSet]

  /** Subset graph: for every Copy-like constraint we record an edge `srcKey → dstKey`. */
  private val subsetOut = mutable.HashMap.empty[String, mutable.HashSet[String]]

  /** Deferred field loads keyed by packed base key. Each entry is `(packedDst, fieldName)`. */
  private val loadsByBase = mutable.HashMap.empty[String, mutable.ArrayBuffer[(String, String)]]

  /** Deferred field stores keyed by packed base key. Each entry is `(fieldName, packedSrc)`. */
  private val storesByBase = mutable.HashMap.empty[String, mutable.ArrayBuffer[(String, String)]]

  /** A virtual call instantiated under a specific caller context, with pre-packed pointer variables. */
  private final case class InstantiatedVirtualCall(
    callerCtx: Int,
    callNodeId: Long,
    receiverK: String,
    methodName: String,
    signature: String,
    argVarsK: Vector[String],
    callResultVarK: String,
    seen: mutable.HashSet[Int]
  )

  /** Deferred virtual calls keyed by packed receiver key. */
  private val vcallsByReceiver = mutable.HashMap.empty[String, mutable.ArrayBuffer[InstantiatedVirtualCall]]

  /** Resolved virtual-dispatch targets keyed by call node id. Read by [[PointerAnalysis]] to rewrite CALL edges. */
  val resolvedCallTargets: mutable.HashMap[Long, mutable.LinkedHashSet[String]] =
    mutable.HashMap.empty

  /** Memoises `(ctx, methodFullName)` pairs we have already instantiated, so recursive cycles terminate. */
  private val instantiated = mutable.HashSet.empty[(Int, String)]

  private val worklist = mutable.Queue.empty[String]

  // ---------------------------------------------------------------------------
  // CPG lookup tables
  // ---------------------------------------------------------------------------

  private val methodByFullName: Map[String, Method] =
    cpg.method.toList.map(m => m.fullName -> m).toMap

  /** Per-type method table for virtual dispatch resolution. */
  private val methodByType: Map[String, Map[(String, String), String]] = {
    cpg.typeDecl.toList.map { td =>
      val entries = td._methodViaAstOut.toList.map { m =>
        (m.name, Option(m.signature).getOrElse("")) -> m.fullName
      }.toMap
      td.fullName -> entries
    }.toMap.withDefaultValue(Map.empty)
  }

  /** Superclass chain (including the type itself) keyed by type full name. */
  private val supertypesOf: Map[String, List[String]] = {
    val result = mutable.HashMap.empty[String, List[String]]
    def compute(tfn: String, seen: Set[String]): List[String] = {
      if (seen.contains(tfn)) Nil
      else
        result.getOrElseUpdate(
          tfn, {
            val parents = cpg.typeDecl
              .fullNameExact(tfn)
              ._typeViaInheritsFromOut
              .referencedTypeDecl
              .fullName
              .toList
            tfn :: parents.flatMap(compute(_, seen + tfn))
          }
        )
    }
    cpg.typeDecl.fullName.foreach(compute(_, Set.empty))
    result.toMap.withDefaultValue(Nil)
  }

  /** Parameter (index, name) lists keyed by method full name — cached once to avoid re-walking the CPG during
    * instantiation.
    */
  private val paramsByMethod: Map[String, Vector[(Int, String)]] =
    methodByFullName.view.mapValues { m =>
      m.parameter.l.sortBy(_.index).map(p => (p.index, p.name)).toVector
    }.toMap

  // ---------------------------------------------------------------------------
  // Solve
  // ---------------------------------------------------------------------------

  def solve(): Map[String, PointsToSet] = {
    // 1. Baseline instantiation in the default context.
    constraintsByMethod.keys.foreach(m => instantiate(DEFAULT_CTX, m))

    // 2. Fixed-point worklist drain.
    while (worklist.nonEmpty) {
      worklistIterations += 1
      val v   = worklist.dequeue()
      val set = pt.getOrElse(v, PointsToSet.empty)
      if (set.nonEmpty) {
        subsetOut.get(v).foreach { outs =>
          outs.foreach { d =>
            val dSet = pt.getOrElseUpdate(d, PointsToSet.empty)
            if (dSet.unionInPlace(set)) enqueue(d)
          }
        }
        loadsByBase.get(v).foreach { entries =>
          entries.foreach { case (dstK, fld) => dischargeLoad(dstK, v, fld) }
        }
        storesByBase.get(v).foreach { entries =>
          entries.foreach { case (fld, srcK) => dischargeStore(v, fld, srcK) }
        }
        vcallsByReceiver.get(v).foreach { entries =>
          // Iterate a snapshot — discharge may append new virtual calls to the same bucket if the callee
          // contains further virtual dispatch on a variable that aliases this receiver.
          entries.toArray.foreach(dischargeVirtualCall)
        }
      }
    }

    pt.toMap
  }

  // ---------------------------------------------------------------------------
  // Instantiation
  // ---------------------------------------------------------------------------

  /** Instantiate a method's constraint template under `ctx`. Memoised. */
  private def instantiate(ctx: Int, methodFullName: String): Unit = {
    if (!instantiated.add((ctx, methodFullName))) return
    val cs = constraintsByMethod.getOrElse(methodFullName, IndexedSeq.empty)
    cs.foreach(interpret(ctx, _))
  }

  private def interpret(ctx: Int, c: Constraint): Unit = c match {
    case Alloc(dst, idx) =>
      val dk  = k(ctx, dst)
      val set = pt.getOrElseUpdate(dk, PointsToSet.empty)
      if (set.add(idx)) enqueue(dk)

    case Copy(dst, src) =>
      addSubsetEdge(k(ctx, src), k(ctx, dst))

    case Load(dst, base, fld) =>
      val bk  = k(ctx, base)
      val dk  = k(ctx, dst)
      loadsByBase.getOrElseUpdate(bk, mutable.ArrayBuffer.empty).append((dk, fld))
      val baseSet = pt.getOrElse(bk, PointsToSet.empty)
      if (baseSet.nonEmpty) dischargeLoad(dk, bk, fld)

    case Store(base, fld, src) =>
      val bk = k(ctx, base)
      val sk = k(ctx, src)
      storesByBase.getOrElseUpdate(bk, mutable.ArrayBuffer.empty).append((fld, sk))
      val baseSet = pt.getOrElse(bk, PointsToSet.empty)
      if (baseSet.nonEmpty) dischargeStore(bk, fld, sk)

    case vc: VirtualCall =>
      val rk = k(ctx, vc.receiver)
      val inst = InstantiatedVirtualCall(
        callerCtx      = ctx,
        callNodeId     = vc.callNodeId,
        receiverK      = rk,
        methodName     = vc.methodName,
        signature      = vc.signature,
        argVarsK       = vc.argVars.map(v => k(ctx, v)),
        callResultVarK = k(ctx, vc.callResultVar),
        seen           = mutable.HashSet.empty
      )
      vcallsByReceiver.getOrElseUpdate(rk, mutable.ArrayBuffer.empty).append(inst)
      val rset = pt.getOrElse(rk, PointsToSet.empty)
      if (rset.nonEmpty) dischargeVirtualCall(inst)

    case sc: StaticCall =>
      val calleeCtx = ctx
      instantiate(calleeCtx, sc.calleeFullName)
      resolvedCallTargets
        .getOrElseUpdate(sc.callNodeId, mutable.LinkedHashSet.empty)
        .add(sc.calleeFullName)
      paramsByMethod.get(sc.calleeFullName).foreach { params =>
        params.foreach { case (idx, pname) =>
          sc.argVars.lift(idx).foreach { argVar =>
            addSubsetEdge(
              k(ctx, argVar),
              k(calleeCtx, PointerVar.local(sc.calleeFullName, pname))
            )
          }
        }
      }
      addSubsetEdge(
        k(calleeCtx, PointerVar.ret(sc.calleeFullName)),
        k(ctx, sc.callResultVar)
      )
  }

  // ---------------------------------------------------------------------------
  // Discharge helpers
  // ---------------------------------------------------------------------------

  // ---------------------------------------------------------------------------
  // Metrics
  // ---------------------------------------------------------------------------

  /** Total worklist iterations performed during [[solve]]. */
  var worklistIterations: Long = 0L

  /** Number of (context, method) pairs instantiated. */
  def contextCount: Int = instantiated.size

  /** Number of pointer variables the solver tracks. */
  def variableCount: Int = pt.size

  /** Number of subset edges in the graph. */
  def subsetEdgeCount: Int = subsetOut.valuesIterator.map(_.size).sum

  private def enqueue(v: String): Unit = worklist.enqueue(v)

  /** Add a subset edge `srcK → dstK` and immediately push whatever is already in `srcK`. */
  private def addSubsetEdge(srcK: String, dstK: String): Unit = {
    val outs = subsetOut.getOrElseUpdate(srcK, mutable.HashSet.empty)
    if (outs.add(dstK)) {
      val srcSet = pt.getOrElse(srcK, PointsToSet.empty)
      if (srcSet.nonEmpty) {
        val dstSet = pt.getOrElseUpdate(dstK, PointsToSet.empty)
        if (dstSet.unionInPlace(srcSet)) enqueue(dstK)
      }
    }
  }

  /** Wire per-type field slots for every type in `pt(baseK)` into `dstK`. */
  private def dischargeLoad(dstK: String, baseK: String, fld: String): Unit = {
    val baseSet = pt.getOrElse(baseK, PointsToSet.empty)
    allocTable.typesOf(baseSet).foreach { t =>
      val slotK = k(DEFAULT_CTX, PointerVar.field(t, fld))
      addSubsetEdge(slotK, dstK)
    }
  }

  private def dischargeStore(baseK: String, fld: String, srcK: String): Unit = {
    val baseSet = pt.getOrElse(baseK, PointsToSet.empty)
    allocTable.typesOf(baseSet).foreach { t =>
      val slotK = k(DEFAULT_CTX, PointerVar.field(t, fld))
      addSubsetEdge(srcK, slotK)
    }
  }

  /** Resolve and instantiate callees for new allocation sites in the receiver's points-to set. */
  private def dischargeVirtualCall(inst: InstantiatedVirtualCall): Unit = {
    val rset    = pt.getOrElse(inst.receiverK, PointsToSet.empty)
    val targets = resolvedCallTargets.getOrElseUpdate(inst.callNodeId, mutable.LinkedHashSet.empty)

    val allocs = rset.iterator.toArray
    allocs.foreach { a =>
      if (inst.seen.add(a)) {
        val t = allocTable.typeOf(a)
        lookupMethod(t, inst.methodName, inst.signature).foreach { calleeFullName =>
          targets.add(calleeFullName)
          val calleeCtx = a
          instantiate(calleeCtx, calleeFullName)
          val params   = paramsByMethod.getOrElse(calleeFullName, Vector.empty)
          val thisName = params.headOption.map(_._2).getOrElse("this")
          val thisK    = k(calleeCtx, PointerVar.local(calleeFullName, thisName))
          val thisSet  = pt.getOrElseUpdate(thisK, PointsToSet.empty)
          if (thisSet.add(a)) enqueue(thisK)
          params.foreach { case (idx, pname) =>
            if (idx >= 1) {
              inst.argVarsK.lift(idx).foreach { argK =>
                addSubsetEdge(argK, k(calleeCtx, PointerVar.local(calleeFullName, pname)))
              }
            }
          }
          addSubsetEdge(k(calleeCtx, PointerVar.ret(calleeFullName)), inst.callResultVarK)
        }
      }
    }
  }

  /** Resolve method by walking the inheritance chain. Falls back to name-only match for generics erasure. */
  private def lookupMethod(typeFullName: String, methodName: String, signature: String): Option[String] = {
    val chain = supertypesOf.getOrElse(typeFullName, List(typeFullName))
    chain.iterator
      .flatMap { t =>
        val tbl = methodByType.getOrElse(t, Map.empty)
        tbl
          .get((methodName, signature))
          .orElse(tbl.collectFirst { case ((n, _), fn) if n == methodName => fn })
      }
      .nextOption()
  }
}
