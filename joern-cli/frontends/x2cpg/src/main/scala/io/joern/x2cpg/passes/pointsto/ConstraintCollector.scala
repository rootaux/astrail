package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Walks the CPG and produces the per-method constraint IR that [[AndersenSolver]] consumes. Extracts allocations,
  * copies, field loads/stores, and call sites. Primitive-typed flows are excluded.
  */
final class ConstraintCollector(cpg: Cpg, diBindings: DiBindings = DiBindings.empty) {

  import DependencyInjectionCollector.ALL_INJECT_ANNOT

  private val byMethod = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Constraint]]
  val allocTable: AllocationSiteTable = new AllocationSiteTable

  /** Counter for synthetic DI allocation-site ids (negative to avoid collisions with real CPG node ids). */
  private var nextSyntheticAllocId: Long = -1L

  private def mintSyntheticAllocId(): Long = {
    val id = nextSyntheticAllocId
    nextSyntheticAllocId -= 1L
    id
  }

  /** Intern a synthetic allocation site (fresh id per call — distinct injection sites get distinct allocations). */
  private def internSyntheticAlloc(typeFullName: String): Int =
    allocTable.intern(mintSyntheticAllocId(), typeFullName)

  /** Emits the full per-method constraint map for the CPG. Must be called once. */
  def collect(): Map[String, IndexedSeq[Constraint]] = {
    cpg.method.foreach(collectFromMethod)
    if (!diBindings.isEmpty) seedFromDiBindings()
    byMethod.view.mapValues(_.toIndexedSeq).toMap
  }

  // -------------------------------------------------------------------------
  // DI-driven seeding
  // -------------------------------------------------------------------------

  /** Emit synthetic Alloc constraints for DI-injected fields, parameters, and registered resource classes. */
  private def seedFromDiBindings(): Unit = {
    seedInjectedFields()
    seedInjectedParameters()
    seedRegisteredResourceClasses()
  }

  /** Synthetic method key for field-level DI allocations. */
  private val DiInitMethod: String = "<di-init>"

  private def seedInjectedFields(): Unit = {
    cpg.typeDecl.foreach { decl =>
      val declType = decl.fullName
      decl.member.foreach { member =>
        if (memberHasInjectAnnotation(member)) {
          val fieldType = Option(member.typeFullName).filter(_.nonEmpty).getOrElse("")
          val impls     = diBindings.implsFor(fieldType)
          if (impls.nonEmpty) {
            val fieldVar = PointerVar.field(declType, member.name)
            impls.foreach { concreteType =>
              val idx = internSyntheticAlloc(concreteType)
              emit(DiInitMethod, Constraint.Alloc(fieldVar, idx))
            }
          }
        }
      }
    }
  }

  private def seedInjectedParameters(): Unit = {
    cpg.method.foreach { method =>
      val methodAnnotated = method.annotation.fullName.exists(ALL_INJECT_ANNOT.contains)
      val isConstructor   = method.name == "<init>"

      method.parameter.foreach { param =>
        val paramAnnotated = param.annotation.fullName.exists(ALL_INJECT_ANNOT.contains)
        if (methodAnnotated || paramAnnotated || (isConstructor && constructorHasInject(method))) {
          val paramType = Option(param.typeFullName).filter(_.nonEmpty).getOrElse("")
          val impls     = diBindings.implsFor(paramType)
          if (impls.nonEmpty) {
            val paramVar = PointerVar.local(method.fullName, param.name)
            impls.foreach { concreteType =>
              val idx = internSyntheticAlloc(concreteType)
              emit(method.fullName, Constraint.Alloc(paramVar, idx))
            }
          }
        }
      }
    }
  }

  /** Seed `this` slots of all methods on reflectively-instantiated framework classes. */
  private def seedRegisteredResourceClasses(): Unit = {
    diBindings.classRegistrations.foreach { resourceType =>
      cpg.typeDecl.fullNameExact(resourceType).foreach { decl =>
        decl.method.foreach { m =>
          val idx = internSyntheticAlloc(resourceType)
          emit(m.fullName, Constraint.Alloc(PointerVar.thisOf(m.fullName), idx))
        }
      }
    }
  }

  private def memberHasInjectAnnotation(member: Member): Boolean =
    member.astChildren.collectAll[Annotation].fullName.exists(ALL_INJECT_ANNOT.contains)

  private def constructorHasInject(method: Method): Boolean =
    method.annotation.fullName.exists(ALL_INJECT_ANNOT.contains)

  private def emit(methodFullName: String, c: Constraint): Unit = {
    byMethod.getOrElseUpdate(methodFullName, mutable.ArrayBuffer.empty).append(c)
  }

  // -------------------------------------------------------------------------
  // Method-level walk
  // -------------------------------------------------------------------------

  private def collectFromMethod(method: Method): Unit = {
    val mfn = method.fullName

    byMethod.getOrElseUpdate(mfn, mutable.ArrayBuffer.empty)
    method.ast.isCall.nameExact(Operators.assignment).foreach { assign =>
      handleAssignment(mfn, assign)
    }

    // Returns
    method.ast.isReturn.foreach { ret =>
      ret.astChildren.collectFirst { case e: Expression => e }.foreach { e =>
        exprVar(mfn, e).foreach { v =>
          emit(mfn, Constraint.Copy(PointerVar.ret(mfn), v))
        }
      }
    }

    // Calls
    method.ast.isCall.filterNot(c => isOperator(c.name)).foreach { call =>
      handleCall(mfn, call)
    }
  }

  // -------------------------------------------------------------------------
  // Assignment handling
  // -------------------------------------------------------------------------

  private def handleAssignment(methodFullName: String, assign: Call): Unit = {
    val args = assign.argument.l
    if (args.size < 2) return
    val lhs = args.head
    val rhs = args(1)

    val lhsVar: Option[String] = lhs match {
      case id: Identifier                                       => Some(PointerVar.local(methodFullName, id.name))
      case fa: Call if fa.name == Operators.fieldAccess         => fieldAccessVar(fa)
      case fa: Call if fa.name == Operators.indirectFieldAccess => fieldAccessVar(fa)
      case _                                                    => None
    }

    rhs match {
      case alloc: Call if isAllocation(alloc) =>
        val idx = allocTable.intern(alloc.id(), allocType(alloc))
        lhsVar.foreach(v => emit(methodFullName, Constraint.Alloc(v, idx)))

      case call: Call if call.name == Operators.fieldAccess || call.name == Operators.indirectFieldAccess =>
        for {
          dst                <- lhsVar
          (baseVar, fldName) <- fieldAccessParts(methodFullName, call)
        } emit(methodFullName, Constraint.Load(dst, baseVar, fldName))

      case call: Call if !isOperator(call.name) =>
        lhsVar.foreach { v =>
          emit(methodFullName, Constraint.Copy(v, PointerVar.callResult(call.id())))
        }

      case id: Identifier =>
        lhsVar.foreach { v =>
          if (isReferenceType(Option(id.typeFullName).getOrElse(""))) {
            emit(methodFullName, Constraint.Copy(v, PointerVar.local(methodFullName, id.name)))
          }
        }

      case _ =>
    }

    lhs match {
      case fa: Call if fa.name == Operators.fieldAccess || fa.name == Operators.indirectFieldAccess =>
        fieldAccessParts(methodFullName, fa).foreach { case (baseVar, fldName) =>
          exprVar(methodFullName, rhs).foreach { srcVar =>
            emit(methodFullName, Constraint.Store(baseVar, fldName, srcVar))
          }
        }
      case _ =>
    }
  }

  // -------------------------------------------------------------------------
  // Call handling
  // -------------------------------------------------------------------------

  private def handleCall(methodFullName: String, call: Call): Unit = {
    val resultVar = PointerVar.callResult(call.id())
    val argVars   = call.argument.toVector.flatMap(exprVar(methodFullName, _))

    call.dispatchType match {
      case DispatchTypes.DYNAMIC_DISPATCH =>
        val receiverVar = call.receiver.headOption.flatMap(exprVar(methodFullName, _))
        receiverVar match {
          case Some(recv) =>
            emit(
              methodFullName,
              Constraint.VirtualCall(
                callNodeId    = call.id(),
                receiver      = recv,
                methodName    = call.name,
                signature     = Option(call.signature).getOrElse(""),
                argVars       = argVars,
                callResultVar = resultVar
              )
            )
          case None =>
            emitStaticCall(methodFullName, call, argVars, resultVar)
        }

      case DispatchTypes.STATIC_DISPATCH =>
        emitStaticCall(methodFullName, call, argVars, resultVar)

      case _ =>
    }
  }

  private def emitStaticCall(
    methodFullName: String,
    call: Call,
    argVars: Vector[String],
    resultVar: String
  ): Unit = {
    if (call.methodFullName == null || call.methodFullName.isEmpty) return
    emit(
      methodFullName,
      Constraint.StaticCall(
        callNodeId     = call.id(),
        calleeFullName = call.methodFullName,
        argVars        = argVars,
        callResultVar  = resultVar
      )
    )
  }

  // -------------------------------------------------------------------------
  // Expression → pointer-variable lowering
  // -------------------------------------------------------------------------

  /** Map an expression to a pointer variable, or `None` for primitives / non-pointer expressions. */
  private def exprVar(methodFullName: String, expr: Expression): Option[String] = expr match {
    case id: Identifier =>
      if (isReferenceType(Option(id.typeFullName).getOrElse("")))
        Some(PointerVar.local(methodFullName, id.name))
      else None
    case p: MethodParameterIn =>
      Some(PointerVar.local(methodFullName, p.name))
    case call: Call if call.name == Operators.fieldAccess || call.name == Operators.indirectFieldAccess =>
      fieldAccessParts(methodFullName, call).map { case (_, fld) => fieldSlotFromAccess(call, fld) }
    case call: Call if !isOperator(call.name) =>
      Some(PointerVar.callResult(call.id()))
    case _ => None
  }

  // -------------------------------------------------------------------------
  // Field-access helpers
  // -------------------------------------------------------------------------

  /** Extract `(baseVar, fieldName)` from a field-access call. */
  private def fieldAccessParts(methodFullName: String, fa: Call): Option[(String, String)] = {
    val as = fa.argument.l
    if (as.size < 2) return None
    val base = as.head
    val fld = as(1) match {
      case fi: FieldIdentifier => fi.canonicalName
      case other               => other.code
    }
    exprVar(methodFullName, base).map(bv => (bv, fld))
  }

  /** Field-slot variable for a value-position field access (uses `*` if base type is unknown). */
  private def fieldAccessVar(fa: Call): Option[String] = {
    val baseType = fa.argument.headOption.map(typeFullNameOf).getOrElse("*")
    val fldName = fa.argument.l.lift(1).map {
      case fi: FieldIdentifier => fi.canonicalName
      case other: AstNode      => other.code
    }
    fldName.map(n => PointerVar.field(baseType, n))
  }

  private def fieldSlotFromAccess(fa: Call, fldName: String): String = {
    val baseType = fa.argument.headOption.map(typeFullNameOf).getOrElse("*")
    PointerVar.field(baseType, fldName)
  }

  /** Extract `typeFullName` from a node, returning `"*"` if unavailable. */
  private def typeFullNameOf(node: AstNode): String = node match {
    case id: Identifier       => Option(id.typeFullName).filter(_.nonEmpty).getOrElse("*")
    case c: Call              => Option(c.typeFullName).filter(_.nonEmpty).getOrElse("*")
    case p: MethodParameterIn => Option(p.typeFullName).filter(_.nonEmpty).getOrElse("*")
    case l: Local             => Option(l.typeFullName).filter(_.nonEmpty).getOrElse("*")
    case lit: Literal         => Option(lit.typeFullName).filter(_.nonEmpty).getOrElse("*")
    case _                    => "*"
  }

  // -------------------------------------------------------------------------
  // Misc predicates
  // -------------------------------------------------------------------------

  private def isOperator(name: String): Boolean = name != null && name.startsWith("<operator>")

  /** True for `<operator>.alloc` or `<init>` calls (both valid allocation sites, deduped by the interning table). */
  private def isAllocation(call: Call): Boolean = {
    val n = call.name
    n == Operators.alloc || (n != null && n == "<init>")
  }

  private def allocType(call: Call): String = {
    val t = Option(call.typeFullName).filter(_.nonEmpty)
    t.orElse(Option(call.methodFullName).map(_.stripSuffix(".<init>"))).getOrElse("ANY")
  }

  /** Conservative: anything that isn't a JVM primitive or void is treated as a reference type. */
  private def isReferenceType(typeFullName: String): Boolean = {
    if (typeFullName == null || typeFullName.isEmpty) return true
    val primitives = Set("byte", "short", "int", "long", "float", "double", "boolean", "char", "void")
    !primitives.contains(typeFullName)
  }
}
