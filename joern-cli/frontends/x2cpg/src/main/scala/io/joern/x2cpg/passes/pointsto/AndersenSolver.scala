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
  * Pointer variables are packed as `"$ctx|$var"` and then interned to dense `Int` ids (see [[intern]]); field slots
  * (prefix `F:`) are always packed under the default context. All hot-path maps are keyed by these int ids, so the
  * fixpoint loop never hashes a string.
  *
  * Cycles in the subset (copy) graph are collapsed online with Lazy Cycle Detection: when propagating along an edge
  * `v → d` leaves `pt(d)` unchanged and equal to `pt(v)`, and `d` can reach `v` in the subset graph, the two are in a
  * strongly connected component and share one final points-to set, so they are merged (union-find, see [[find]] /
  * [[collapse]]). This avoids churning the same allocation sites around a cycle O(cycle length) times.
  *
  * Fixed point is reached because all operations are monotone (sets only grow, instantiation is memoised, and a
  * collapse strictly reduces the number of live variables).
  */
final class AndersenSolver(
  cpg: Cpg,
  allocTable: AllocationSiteTable,
  constraintsByMethod: Map[String, IndexedSeq[Constraint]]
) {

  import Constraint.*

  /** Default context key, used for baseline instantiation and context-insensitive field slots. */
  private val DEFAULT_CTX: Int = -1

  // ---------------------------------------------------------------------------
  // Pointer-variable interning: packed "$ctx|$var" string -> dense int id.
  // The string is built (and hashed) once, at intern time; every subsequent map
  // access on the hot path uses the int id, resolved through the union-find.
  // ---------------------------------------------------------------------------

  private val varIdOf = mutable.HashMap.empty[String, Int]
  private val varKeys = mutable.ArrayBuffer.empty[String]

  @inline private def intern(packed: String): Int =
    varIdOf.getOrElseUpdate(packed, { val i = varKeys.size; varKeys += packed; i })

  /** Pack a pointer variable under a context, intern it to an int id, and resolve to its union-find representative.
    * Field slots always use [[DEFAULT_CTX]].
    */
  @inline private def k(ctx: Int, v: String): Int =
    find(intern(if (v.startsWith("F:")) s"$DEFAULT_CTX|$v" else s"$ctx|$v"))

  // ---------------------------------------------------------------------------
  // Union-find over pointer-variable ids (cycle elimination).
  // parent(x) absent => x is its own representative.
  // ---------------------------------------------------------------------------

  private val parent = mutable.HashMap.empty[Int, Int]

  /** Representative of `x`'s equivalence class, with path compression. */
  private def find(x: Int): Int = {
    var root = x
    var p    = parent.getOrElse(root, root)
    while (p != root) { root = p; p = parent.getOrElse(root, root) }
    var cur = x
    while (cur != root) { val nxt = parent.getOrElse(cur, cur); parent(cur) = root; cur = nxt }
    root
  }

  /** Points-to map: representative variable id → set of allocation-site indices. */
  private val pt = mutable.HashMap.empty[Int, PointsToSet]

  /** Subset graph: for every Copy-like constraint we record an edge `srcId → dstId` (endpoints are representatives). */
  private val subsetOut = mutable.HashMap.empty[Int, mutable.HashSet[Int]]

  /** Deferred field loads keyed by base id. Each entry is `(dstId, fieldName)`. */
  private val loadsByBase = mutable.HashMap.empty[Int, mutable.ArrayBuffer[(Int, String)]]

  /** Deferred field stores keyed by base id. Each entry is `(fieldName, srcId)`. */
  private val storesByBase = mutable.HashMap.empty[Int, mutable.ArrayBuffer[(String, Int)]]

  /** A virtual call instantiated under a specific caller context, with pre-interned pointer variables. */
  private final case class InstantiatedVirtualCall(
    callerCtx: Int,
    callNodeId: Long,
    receiverK: Int,
    methodName: String,
    signature: String,
    argVarsK: Vector[Int],
    callResultVarK: Int,
    seen: mutable.HashSet[Int]
  )

  /** Deferred virtual calls keyed by receiver id. */
  private val vcallsByReceiver = mutable.HashMap.empty[Int, mutable.ArrayBuffer[InstantiatedVirtualCall]]

  /** Resolved virtual-dispatch targets keyed by call node id. Read by [[PointerAnalysis]] to rewrite CALL edges. */
  val resolvedCallTargets: mutable.HashMap[Long, mutable.LinkedHashSet[String]] =
    mutable.HashMap.empty

  /** Memoises `(ctx, methodFullName)` pairs we have already instantiated, so recursive cycles terminate. */
  private val instantiated = mutable.HashSet.empty[(Int, String)]

  private val worklist = mutable.Queue.empty[Int]

  /** Representatives currently queued in `worklist`. Prevents enqueuing (and later fully re-processing) the same hot
    * variable multiple times. Cleared on dequeue *before* processing, so a re-enqueue during processing still takes
    * effect.
    */
  private val onWorklist = mutable.HashSet.empty[Int]

  /** Per-variable record of the allocation-site indices already propagated out of the variable along its
    * subset edges. Enables difference (delta) propagation: only newly-arrived indices are pushed each fire.
    */
  private val propagated = mutable.HashMap.empty[Int, mutable.BitSet]

  /** Per-base record of the allocation-site indices already wired into field slots for that base's deferred
    * loads/stores. Lets the fixpoint discharge only the newly-arrived allocations' types each fire instead of
    * re-scanning the whole base set and rebuilding a `Set[String]` of types every time.
    */
  private val dischargedBaseAllocs = mutable.HashMap.empty[Int, mutable.BitSet]

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
      val v   = find(worklist.dequeue())
      onWorklist.remove(v)
      val set = pt.getOrElse(v, PointsToSet.empty)
      if (set.nonEmpty) {
        // Difference propagation: push only the indices of pt(v) not yet propagated out of v along its
        // subset edges (recorded in `propagated(v)`), instead of re-unioning the whole set every fire.
        val prop  = propagated.getOrElseUpdate(v, mutable.BitSet.empty)
        val delta = set.diffBits(prop)
        if (delta.nonEmpty) {
          prop |= delta
          var cycleCandidates: List[Int] = Nil
          subsetOut.get(v).foreach { outs =>
            outs.foreach { d0 =>
              val d = find(d0)
              if (d != v) {
                val dSet = pt.getOrElseUpdate(d, PointsToSet.empty)
                if (dSet.absorb(delta)) enqueue(d)
                else if (dSet.bits == set.bits && reaches(d, v)) {
                  // v → d, d reaches v, and both sets are equal: v and d are in the same SCC.
                  cycleCandidates = d :: cycleCandidates
                }
              }
            }
          }
          // Collapse after iterating (collapse mutates subsetOut(v)).
          cycleCandidates.foreach(d => collapse(v, d))
        }
        // Field discharge on base growth: wire only the types of the newly-arrived allocations into every
        // deferred load/store slot, instead of re-scanning the whole base set on every fire (addSubsetEdge is
        // idempotent, so re-deriving already-wired edges is pure waste). A newly *added* load/store constraint
        // still gets the full current base set once, via dischargeLoad/dischargeStore in interpret.
        val loadEntries  = loadsByBase.get(v)
        val storeEntries = storesByBase.get(v)
        if (loadEntries.isDefined || storeEntries.isDefined) {
          val done      = dischargedBaseAllocs.getOrElseUpdate(v, mutable.BitSet.empty)
          val newAllocs = set.diffBits(done)
          if (newAllocs.nonEmpty) {
            done |= newAllocs
            val newTypes = newAllocs.iterator.map(allocTable.typeOf).toSet
            loadEntries.foreach(_.foreach { case (dstK, fld) =>
              val d = find(dstK)
              newTypes.foreach(t => addSubsetEdge(k(DEFAULT_CTX, PointerVar.field(t, fld)), d))
            })
            storeEntries.foreach(_.foreach { case (fld, srcK) =>
              val s = find(srcK)
              newTypes.foreach(t => addSubsetEdge(s, k(DEFAULT_CTX, PointerVar.field(t, fld))))
            })
          }
        }
        vcallsByReceiver.get(v).foreach { entries =>
          // Iterate a snapshot — discharge may append new virtual calls to the same bucket if the callee
          // contains further virtual dispatch on a variable that aliases this receiver.
          entries.toArray.foreach(dischargeVirtualCall)
        }
      }
    }

    // Re-key the result back to the packed variable strings for consumers ([[PointerAnalysis]]).
    pt.iterator.map { case (id, set) => varKeys(id) -> set }.toMap
  }

  /** True if `target` is reachable from `from` following subset edges (both resolved through the union-find). */
  private def reaches(from: Int, target: Int): Boolean = {
    val goal    = find(target)
    val stack   = mutable.ArrayDeque[Int](find(from))
    val visited = mutable.HashSet.empty[Int]
    while (stack.nonEmpty) {
      val n = find(stack.removeLast())
      if (n == goal) return true
      if (visited.add(n)) subsetOut.get(n).foreach(_.foreach(t => stack.append(find(t))))
    }
    false
  }

  /** Merge the strongly-connected representatives `a` and `b` into one (b folded into a). */
  private def collapse(a0: Int, b0: Int): Unit = {
    val a = find(a0)
    val b = find(b0)
    if (a == b) return
    parent(b) = a
    pt.remove(b).foreach(bSet => pt.getOrElseUpdate(a, PointsToSet.empty).absorb(bSet.bits))
    subsetOut.remove(b).foreach { bOuts =>
      val aOuts = subsetOut.getOrElseUpdate(a, mutable.HashSet.empty)
      bOuts.foreach { t => val ft = find(t); if (ft != a) aOuts.add(ft) }
    }
    subsetOut.get(a).foreach(_.remove(a)) // drop any self-loop created by the merge
    loadsByBase.remove(b).foreach(e => loadsByBase.getOrElseUpdate(a, mutable.ArrayBuffer.empty) ++= e)
    storesByBase.remove(b).foreach(e => storesByBase.getOrElseUpdate(a, mutable.ArrayBuffer.empty) ++= e)
    vcallsByReceiver.remove(b).foreach(e => vcallsByReceiver.getOrElseUpdate(a, mutable.ArrayBuffer.empty) ++= e)
    // Conservative: reset the delta bookkeeping for the merged node and re-propagate its full set once.
    propagated.remove(a)
    propagated.remove(b)
    dischargedBaseAllocs.remove(a)
    dischargedBaseAllocs.remove(b)
    onWorklist.remove(b)
    enqueue(a)
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

  /** Number of pointer variables the solver tracks (live representatives). */
  def variableCount: Int = pt.size

  /** Number of subset edges in the graph. */
  def subsetEdgeCount: Int = subsetOut.valuesIterator.map(_.size).sum

  private def enqueue(v0: Int): Unit = {
    val v = find(v0)
    if (onWorklist.add(v)) worklist.enqueue(v)
  }

  /** Add a subset edge `srcK → dstK` and immediately push whatever is already in `srcK`. Endpoints are resolved. */
  private def addSubsetEdge(srcK0: Int, dstK0: Int): Unit = {
    val srcK = find(srcK0)
    val dstK = find(dstK0)
    if (srcK == dstK) return
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
  private def dischargeLoad(dstK: Int, baseK: Int, fld: String): Unit = {
    val baseSet = pt.getOrElse(find(baseK), PointsToSet.empty)
    allocTable.typesOf(baseSet).foreach { t =>
      addSubsetEdge(k(DEFAULT_CTX, PointerVar.field(t, fld)), dstK)
    }
  }

  private def dischargeStore(baseK: Int, fld: String, srcK: Int): Unit = {
    val baseSet = pt.getOrElse(find(baseK), PointsToSet.empty)
    allocTable.typesOf(baseSet).foreach { t =>
      addSubsetEdge(srcK, k(DEFAULT_CTX, PointerVar.field(t, fld)))
    }
  }

  /** Resolve and instantiate callees for new allocation sites in the receiver's points-to set. */
  private def dischargeVirtualCall(inst: InstantiatedVirtualCall): Unit = {
    val rset    = pt.getOrElse(find(inst.receiverK), PointsToSet.empty)
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
                addSubsetEdge(find(argK), k(calleeCtx, PointerVar.local(calleeFullName, pname)))
              }
            }
          }
          addSubsetEdge(k(calleeCtx, PointerVar.ret(calleeFullName)), find(inst.callResultVarK))
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
