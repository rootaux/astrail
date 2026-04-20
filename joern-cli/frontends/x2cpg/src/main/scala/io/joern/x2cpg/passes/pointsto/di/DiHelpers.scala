package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Shared helpers for framework-specific DI collectors: class-literal extraction, allocation-type detection,
  * and type-hierarchy traversal.
  */
trait DiHelpers {

  protected def cpg: Cpg

  protected val bindings: mutable.LinkedHashMap[String, mutable.LinkedHashSet[String]] =
    mutable.LinkedHashMap.empty

  protected val classRegistrations: mutable.LinkedHashSet[String] =
    mutable.LinkedHashSet.empty

  protected def addBinding(key: String, impl: String): Unit = {
    if (key == null || key.isEmpty || impl == null || impl.isEmpty) return
    bindings.getOrElseUpdate(key, mutable.LinkedHashSet.empty).add(impl)
  }

  // -------------------------------------------------------------------------
  // Class literal extraction
  // -------------------------------------------------------------------------

  /** Extract the type from a class literal (`Foo.class` / `TypeRef`). */
  protected def classLiteralType(exprOpt: Option[AstNode]): Option[String] = exprOpt.flatMap {
    case tr: TypeRef =>
      Option(tr.typeFullName).filter(_.nonEmpty)
    case call: Call if call.name == Operators.fieldAccess || call.name == Operators.indirectFieldAccess =>
      val fld = call.argument.argumentIndex(2).headOption
      val isClassField = fld.exists {
        case fi: FieldIdentifier => fi.canonicalName == "class"
        case other: AstNode      => other.code == "class"
      }
      if (!isClassField) None
      else
        call.argument.argumentIndex(1).headOption.flatMap {
          case id: Identifier => Option(id.typeFullName).filter(_.nonEmpty).filter(_ != "ANY")
          case tr: TypeRef    => Option(tr.typeFullName).filter(_.nonEmpty)
          case _              => None
        }
    case id: Identifier =>
      Option(id.typeFullName).filter(_.nonEmpty).filter(_ != "ANY")
    case _ => None
  }

  // -------------------------------------------------------------------------
  // Allocation type extraction
  // -------------------------------------------------------------------------

  /** Returns the concrete type of an allocation expression (`new Foo()` → `Some("Foo")`). */
  protected def allocationTypeOf(node: AstNode): Option[String] = node match {
    case call: Call if call.name == Operators.alloc =>
      Option(call.typeFullName).filter(_.nonEmpty)
    case call: Call if call.name == "<init>" =>
      Option(call.methodFullName).map(_.stripSuffix(".<init>")).filter(_.nonEmpty)
    case call: Call if call.name == Operators.assignment =>
      call.argument.argumentIndex(2).headOption.flatMap(allocationTypeOf)
    case block: Block =>
      block.astChildren.collectFirst(Function.unlift(allocationTypeOf))
    case _ => None
  }

  // -------------------------------------------------------------------------
  // Type hierarchy
  // -------------------------------------------------------------------------

  /** Returns all ancestor type full names of `decl` (transitive). */
  protected def ancestorTypeFullNames(decl: TypeDecl): Set[String] = {
    val seen = mutable.HashSet.empty[String]
    val work = mutable.Stack.empty[TypeDecl]

    def pushParentsOf(td: TypeDecl): Unit = {
      td.inheritsFromTypeFullName.foreach(seen.add)
      td.baseType.referencedTypeDecl.foreach { parentDecl =>
        if (!seen.contains(parentDecl.fullName)) work.push(parentDecl)
      }
    }

    pushParentsOf(decl)
    while (work.nonEmpty) {
      val cur = work.pop()
      if (seen.add(cur.fullName)) pushParentsOf(cur)
    }
    seen.toSet
  }
}
