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
          val fieldVar  = PointerVar.field(declType, member.name)
          if (isCollectionType(fieldType)) {
            // Collection injection (`@Inject List<Handler>`): Spring injects every bean of the generic element
            // type. Model the field as a synthetic collection whose element slot holds each impl, so reads
            // (list.get(), etc.) resolve via the same collection element machinery.
            collectionElementType(member).foreach { elementType =>
              val impls = diBindings.implsFor(elementType)
              if (impls.nonEmpty) {
                emit(DiInitMethod, Constraint.Alloc(fieldVar, internSyntheticAlloc(fieldType)))
                val elemSlot = PointerVar.field(fieldType, CollElem)
                impls.foreach(impl => emit(DiInitMethod, Constraint.Alloc(elemSlot, internSyntheticAlloc(impl))))
              }
            }
          } else {
            val allImpls = diBindings.implsFor(fieldType)
            // @Qualifier("name") pins the injection to one bean: keep only the impl whose bean name matches.
            // Fall back to all impls if the qualifier names a bean we can't resolve (e.g. explicit @Component name),
            // so precision never costs recall.
            val impls = qualifierValue(member) match {
              case Some(qual) =>
                val matched = allImpls.filter(impl => defaultBeanName(impl) == qual)
                if (matched.nonEmpty) matched else allImpls
              case None => allImpls
            }
            impls.foreach(concreteType => emit(DiInitMethod, Constraint.Alloc(fieldVar, internSyntheticAlloc(concreteType))))
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
        if (
          methodAnnotated || paramAnnotated || (isConstructor && constructorHasInject(method)) ||
          isSoleConstructorInjection(method)
        ) {
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

  /** Types the DI collectors treat as framework-managed beans (registered classes and every bound impl). */
  private lazy val managedTypes: Set[String] =
    diBindings.classRegistrations ++ diBindings.interfaceToImpls.values.flatten.toSet

  /** Spring 4.3+ implicit constructor injection: a DI-managed class with exactly one constructor has that
    * constructor's parameters autowired even without an @Autowired annotation. */
  private def isSoleConstructorInjection(method: Method): Boolean =
    method.name == "<init>" &&
      method.typeDecl.fullName.headOption.exists(managedTypes.contains) &&
      method.typeDecl.method.nameExact("<init>").size == 1

  private val QualifierAnnot = "org.springframework.beans.factory.annotation.Qualifier"

  /** The value of a member's `@Qualifier("name")` annotation, if present. */
  private def qualifierValue(member: Member): Option[String] =
    member.astChildren
      .collectAll[Annotation]
      .filter(_.fullName == QualifierAnnot)
      .flatMap(_.parameterAssign.code)
      .headOption
      .map(_.stripPrefix("\"").stripSuffix("\""))

  /** Default Spring bean name of a type: its simple name decapitalised (RealGreeter -> realGreeter). */
  private def defaultBeanName(implFullName: String): String = {
    val simple = implFullName.substring(implFullName.lastIndexOf('.') + 1)
    if (simple.isEmpty) simple else simple.head.toLower.toString + simple.tail
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
        staticFieldSlot(call) match {
          case Some(slot) => lhsVar.foreach(dst => emit(methodFullName, Constraint.Copy(dst, slot)))
          case None =>
            for {
              dst                <- lhsVar
              (baseVar, fldName) <- fieldAccessParts(methodFullName, call)
            } emit(methodFullName, Constraint.Load(dst, baseVar, fldName))
        }

      case call: Call if call.name == Operators.cast =>
        // `lhs = (T) operand` — a cast preserves points-to, so copy from the cast operand into lhs.
        for {
          v      <- lhsVar
          srcVar <- exprVar(methodFullName, call)
        } emit(methodFullName, Constraint.Copy(v, srcVar))

      case call: Call if call.name == Operators.indexAccess =>
        // `lhs = a[i]` — load the array's synthetic element slot into lhs.
        for {
          dst     <- lhsVar
          baseVar <- call.argument.headOption.flatMap(exprVar(methodFullName, _))
        } emit(methodFullName, Constraint.Load(dst, baseVar, ArrayElem))

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

      case mr: MethodRef =>
        // `lhs = () -> ...` / `lhs = Foo::bar` — copy the functional object into lhs.
        lhsVar.foreach(v => emit(methodFullName, Constraint.Copy(v, methodRefVar(methodFullName, mr))))

      case block: Block =>
        // RHS wrapped in a block (e.g. a lambda with capture setup, or a nested new): copy from its value.
        for {
          v      <- lhsVar
          srcVar <- exprVar(methodFullName, block)
        } emit(methodFullName, Constraint.Copy(v, srcVar))

      case _ =>
    }

    lhs match {
      case fa: Call if fa.name == Operators.fieldAccess || fa.name == Operators.indirectFieldAccess =>
        staticFieldSlot(fa) match {
          case Some(slot) =>
            exprVar(methodFullName, rhs).foreach(srcVar => emit(methodFullName, Constraint.Copy(slot, srcVar)))
          case None =>
            fieldAccessParts(methodFullName, fa).foreach { case (baseVar, fldName) =>
              exprVar(methodFullName, rhs).foreach { srcVar =>
                emit(methodFullName, Constraint.Store(baseVar, fldName, srcVar))
              }
            }
        }
      case ia: Call if ia.name == Operators.indexAccess =>
        // `a[i] = x` — store x into the array's synthetic element slot.
        for {
          baseVar <- ia.argument.headOption.flatMap(exprVar(methodFullName, _))
          srcVar  <- exprVar(methodFullName, rhs)
        } emit(methodFullName, Constraint.Store(baseVar, ArrayElem, srcVar))
      case _ =>
    }
  }

  // -------------------------------------------------------------------------
  // Call handling
  // -------------------------------------------------------------------------

  private def handleCall(methodFullName: String, call: Call): Unit = {
    val resultVar = PointerVar.callResult(call.id())

    // Reflection: `Class.forName("Foo").newInstance()` produces a Foo; the JDK newInstance returns Object with no
    // allocation, so model the result as an alloc of the named type. Handled before dispatch (the virtual call
    // into java.lang.Class would resolve to nothing anyway).
    reflectiveAllocType(call) match {
      case Some(t) =>
        emit(methodFullName, Constraint.Alloc(resultVar, allocTable.intern(call.id(), t)))
        return
      case None =>
    }

    // ServiceLoader.load(Foo.class): the result is a collection whose elements are the service interface's impls.
    // META-INF/services is not in the CPG, so fall back to every impl of the interface (CHA).
    if (emitServiceLoaderLoad(methodFullName, call, resultVar)) return

    // Key argument pointer variables by their Joern argumentIndex (receiver/this = 0, explicit args from 1),
    // which matches MethodParameterIn.index. Keeping the index survives the flatMap that drops primitive args,
    // so the solver can bind each argument to the correct parameter.
    val argVars = call.argument.l.flatMap(a => exprVar(methodFullName, a).map(a.argumentIndex -> _)).toMap

    call.dispatchType match {
      case DispatchTypes.DYNAMIC_DISPATCH =>
        val receiverVar = call.receiver.headOption.flatMap(exprVar(methodFullName, _))
        receiverVar match {
          case Some(recv) =>
            if (!emitCollectionAccess(methodFullName, call, recv, resultVar))
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
    argVars: Map[Int, String],
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

  /** Model a JDK collection/map access as a store into, or load from, the receiver's synthetic element slot.
    * Returns true if the call was recognised and handled (so no virtual call is emitted for it). The JDK has no
    * method bodies in the CPG, so these calls would otherwise resolve to nothing.
    */
  private def emitCollectionAccess(methodFullName: String, call: Call, recv: String, resultVar: String): Boolean = {
    val recvType = call.receiver.headOption.map(typeFullNameOf).getOrElse("")
    if (!isCollectionType(recvType)) false
    else if (CollectionStoreMethods.contains(call.name)) {
      // The stored element is the last reference argument (the value for put(key, value)); the receiver is
      // argumentIndex 0 and is excluded.
      call.argument.l
        .filter(_.argumentIndex > 0)
        .reverse
        .flatMap(exprVar(methodFullName, _))
        .headOption
        .foreach(argVar => emit(methodFullName, Constraint.Store(recv, CollElem, argVar)))
      true
    } else if (CollectionLoadMethods.contains(call.name)) {
      emit(methodFullName, Constraint.Load(resultVar, recv, CollElem))
      true
    } else if (CollectionPassthroughMethods.contains(call.name)) {
      // iterator()/stream()/... carries the same elements: pass the container through so next()/get() on the
      // result reads the same element slot.
      emit(methodFullName, Constraint.Copy(resultVar, recv))
      true
    } else false
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
      // Static field `Type.field` reads its type-scoped slot directly; an instance field reads through the base's
      // concrete types (the same slot writes use), not the declared-type slot, so a value stored into
      // F:<concreteType>:f is read back even when the base is polymorphic or its type is unknown.
      staticFieldSlot(call).orElse {
        fieldAccessParts(methodFullName, call).map { case (baseVar, fld) =>
          val v = PointerVar.callResult(call.id())
          emit(methodFullName, Constraint.Load(v, baseVar, fld))
          v
        }
      }
    case call: Call if isAllocation(call) =>
      // A direct `new T(...)` in value position (e.g. a store RHS, which lowers to an <init>/alloc call rather
      // than a block): model the allocation into a synthetic variable so it flows like any other reference.
      Some(allocVar(methodFullName, call))
    case call: Call if call.name == Operators.cast =>
      // A cast is identity for points-to: `(Foo) bar` holds the same object as `bar`. The operand is the last
      // argument (the target type is a TypeRef in argument position 1), so map the cast to the operand.
      call.argument.l.lastOption.flatMap(exprVar(methodFullName, _))
    case call: Call if call.name == Operators.indexAccess =>
      // `a[i]` in value position reads the array's synthetic element slot (all elements aliased, index-insensitive).
      call.argument.headOption.map(base => PointerVar.field(typeFullNameOf(base), ArrayElem))
    case call: Call if !isOperator(call.name) =>
      Some(PointerVar.callResult(call.id()))
    case block: Block =>
      // `new Foo(...)` lowers to a Block whose last child is the tmp identifier holding the freshly allocated
      // object; the block's inner `$tmp = <operator>.alloc` assignment is already turned into an Alloc by
      // handleAssignment. Mapping the block to that identifier lets allocations in argument / return / nested
      // position flow into points-to sets, so factory/builder results resolve instead of being dropped.
      block.astChildren.collect { case e: Expression => e }.toList.lastOption
        .flatMap(exprVar(methodFullName, _))
    case mr: MethodRef =>
      Some(methodRefVar(methodFullName, mr))
    case _ => None
  }

  /** A lambda or method reference is an allocation of a functional object. Its allocation type is the synthetic
    * lambda method's own full name (Joern sets MethodRef.typeFullName to it); the solver resolves any functional
    * dispatch on it (run/apply/get/accept/...) to that method. Returns the pointer variable holding it.
    */
  /** Model a direct allocation call into a synthetic pointer variable and return it. */
  private def allocVar(methodFullName: String, alloc: Call): String = {
    val v   = PointerVar.callResult(alloc.id())
    val idx = allocTable.intern(alloc.id(), allocType(alloc))
    emit(methodFullName, Constraint.Alloc(v, idx))
    v
  }

  private def methodRefVar(methodFullName: String, mr: MethodRef): String = {
    val v = PointerVar.callResult(mr.id())
    // Use methodFullName: it is always the target/lambda method. (typeFullName equals it for lambdas but is the
    // functional interface for method references, on which the JDK dispatch would not resolve.)
    val target = Option(mr.methodFullName).filter(_.nonEmpty)
      .orElse(Option(mr.typeFullName).filter(_.nonEmpty))
      .getOrElse("ANY")
    val idx = allocTable.intern(mr.id(), target)
    emit(methodFullName, Constraint.Alloc(v, idx))
    v
  }

  // -------------------------------------------------------------------------
  // Field-access helpers
  // -------------------------------------------------------------------------

  /** Extract `(baseVar, fieldName)` from a field-access call. */
  /** Static field access `Type.field`: the base is a TypeRef (a type, not an object), so the slot is scoped to
    * that type directly (F:Type:field) rather than resolved through a base object's points-to, which is empty.
    * Returns the type-scoped field slot, or None for an ordinary instance field access.
    */
  /** Short type names in the CPG, to recognise a static field access whose base Joern models as an Identifier
    * (e.g. `Holder.INSTANCE`) rather than a TypeRef. */
  private lazy val knownTypeNames: Set[String] = cpg.typeDecl.name.toSet

  private def staticFieldSlot(fa: Call): Option[String] = {
    fa.argument.l match {
      case base :: fldNode :: _ =>
        val fld = fldNode match {
          case fi: FieldIdentifier => fi.canonicalName
          case o: AstNode          => o.code
        }
        base match {
          case tr: TypeRef =>
            Option(tr.typeFullName).filter(_.nonEmpty).map(t => PointerVar.field(t, fld))
          case id: Identifier if knownTypeNames.contains(id.name) =>
            // Base is a type name, not a variable: a static access. Scope the slot to that type.
            Some(PointerVar.field(Option(id.typeFullName).filter(_.nonEmpty).getOrElse(id.name), fld))
          case _ => None
        }
      case _ => None
    }
  }

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

  /** Synthetic field name for array elements. Arrays are modelled index-insensitively: every element aliases one
    * slot, so `a[i] = x; y = a[j]` makes `y` point to `x`.
    */
  private val ArrayElem = "[]"

  /** Synthetic field name for the contents of a JDK collection/map, modelled element-insensitively (one slot). */
  private val CollElem = "<collElem>"

  /** Collection mutators that insert a reference into the container (the last reference argument is the element;
    * for `put(key, value)` this is the value). */
  private val CollectionStoreMethods =
    Set("add", "offer", "push", "put", "set", "addFirst", "addLast", "offerFirst", "offerLast", "addElement")

  /** Collection accessors that return an element from the container. */
  private val CollectionLoadMethods =
    Set("get", "poll", "peek", "remove", "pop", "element", "getFirst", "getLast", "peekFirst", "peekLast",
      "pollFirst", "pollLast", "next")

  /** Accessors that return a view carrying the same elements (an Iterator/Stream/Spliterator). Modelled as a
    * pass-through of the container so a following next()/get() reads the same element slot. */
  private val CollectionPassthroughMethods =
    Set("iterator", "listIterator", "stream", "parallelStream", "spliterator")

  /** Conservative, name-based check for a JDK collection/map receiver (the JDK is not in the CPG, so the type
    * hierarchy is unavailable). Matches java.util.* or a simple name of a core collection interface. */
  private def isCollectionType(typeFullName: String): Boolean = {
    if (typeFullName == null || typeFullName.isEmpty) return false
    // Functional interfaces live under java.util.function.* and are NOT collections; their accessors (get/apply/...)
    // must stay virtual dispatches so lambdas/method refs resolve, not be treated as collection loads.
    if (typeFullName.startsWith("java.util.function.")) return false
    typeFullName.startsWith("java.util.") || {
      val simple = typeFullName.substring(typeFullName.lastIndexOf('.') + 1)
      Set("List", "Map", "Set", "Collection", "Queue", "Deque", "Iterable").contains(simple)
    }
  }

  /** The generic element type of a collection-typed member, from its JVM generic signature
    * (`Ljava.util.List<LHandler;>;` → `Handler`); `member.typeFullName` is the erased raw type. */
  private val GenericElementRe = """<L([^;<>]+);""".r
  private def collectionElementType(member: Member): Option[String] =
    Option(member.genericSignature).flatMap(GenericElementRe.findFirstMatchIn).map(_.group(1))

  /** Reflective instantiation `X.newInstance()` where X is a `Class.forName("Foo")` result, possibly via
    * `getDeclaredConstructor()` / `getConstructor()` — returns the named type from the forName string literal. */
  private def reflectiveAllocType(call: Call): Option[String] =
    if (call.name != "newInstance") None
    else call.receiver.headOption.flatMap(classNameFromReflectiveChain)

  private def classNameFromReflectiveChain(node: Expression): Option[String] = node match {
    case fn: Call if fn.name == "forName" =>
      fn.argument.collectAll[Literal].headOption.map(_.code.stripPrefix("\"").stripSuffix("\""))
    case ctor: Call if ctor.name == "getDeclaredConstructor" || ctor.name == "getConstructor" =>
      ctor.receiver.headOption.flatMap(classNameFromReflectiveChain)
    case _ => None
  }

  /** Concrete implementations of a type, from the CPG's inheritance edges (CHA). */
  private lazy val implsByInterface: Map[String, Set[String]] =
    cpg.typeDecl.l
      .flatMap(td => td.inheritsFromTypeFullName.map(_ -> td.fullName))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap

  /** The type named by a `Foo.class` literal (or a Class-typed identifier). */
  private def classLiteralType(node: AstNode): Option[String] = node match {
    case tr: TypeRef => Option(tr.typeFullName).filter(_.nonEmpty)
    case fa: Call if fa.name == Operators.fieldAccess =>
      val isClassField = fa.argument.argumentIndex(2).headOption.exists {
        case fi: FieldIdentifier => fi.canonicalName == "class"
        case o: AstNode          => o.code == "class"
      }
      if (!isClassField) None
      else
        fa.argument.argumentIndex(1).headOption.flatMap {
          case id: Identifier => Option(id.typeFullName).filter(t => t.nonEmpty && t != "ANY")
          case tr: TypeRef    => Option(tr.typeFullName).filter(_.nonEmpty)
          case _              => None
        }
    case _ => None
  }

  /** `ServiceLoader.load(Foo.class)` — model the result as a collection whose element slot holds every impl of the
    * service interface (CHA). Returns true if handled. */
  private def emitServiceLoaderLoad(methodFullName: String, call: Call, resultVar: String): Boolean = {
    if (!Option(call.methodFullName).exists(_.startsWith("java.util.ServiceLoader.load"))) return false
    call.argument.l.flatMap(classLiteralType).headOption match {
      case Some(iface) =>
        val loaderType = "java.util.ServiceLoader"
        emit(methodFullName, Constraint.Alloc(resultVar, allocTable.intern(call.id(), loaderType)))
        val elemSlot = PointerVar.field(loaderType, CollElem)
        implsByInterface.getOrElse(iface, Set.empty).foreach { impl =>
          emit(methodFullName, Constraint.Alloc(elemSlot, internSyntheticAlloc(impl)))
        }
        true
      case None => false
    }
  }

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
