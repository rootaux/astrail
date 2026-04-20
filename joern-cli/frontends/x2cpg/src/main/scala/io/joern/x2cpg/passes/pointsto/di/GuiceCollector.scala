package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.pointsto.DiBindings
import org.slf4j.LoggerFactory

/** Collects DI bindings from Google Guice / Dagger patterns:
  *
  *   - `AbstractModule.configure()`: `bind(Iface.class).to(Impl.class)`, `bind(Iface.class).toInstance(singleton)`
  *   - `@Provides` factory methods: return type → concrete allocation in method body
  *
  * Guice's `bind(interface).to(impl)` has the interface as the argument to `bind` and the implementation as the
  * argument to `to` — this is the inverse of HK2's convention.
  */
final class GuiceCollector(override protected val cpg: Cpg) extends DiHelpers {

  private val logger = LoggerFactory.getLogger(classOf[GuiceCollector])

  def collect(): DiBindings = {
    cpg.method.nameExact("configure").foreach(processConfigureMethod)
    cpg.method.where(_.annotation.fullNameExact(PROVIDES_ANNOT*)).foreach(processProvidesMethod)

    val result = DiBindings(
      interfaceToImpls = bindings.view.mapValues(_.toSet).toMap,
      classRegistrations = classRegistrations.toSet
    )
    if (!result.isEmpty)
      logger.debug(s"Guice collector: ${result.interfaceToImpls.size} bindings")
    result
  }

  // -------------------------------------------------------------------------
  // AbstractModule.configure()
  // -------------------------------------------------------------------------

  private def processConfigureMethod(method: Method): Unit = {
    method.typeDecl.headOption.foreach { decl =>
      val ancestry = ancestorTypeFullNames(decl)
      if (ancestry.exists(isGuiceModuleType)) {
        method.ast.isCall.foreach(processBindingCall)
      }
    }
  }

  private def processBindingCall(call: Call): Unit = call.name match {
    case "to" =>
      // bind(Iface.class).to(Impl.class)
      val receiverBind = call.receiver.collectAll[Call].headOption
        .orElse(call.argument.argumentIndex(0).collectAll[Call].headOption)
      val implType = classLiteralType(call.argument.argumentIndex(1).headOption)
      (receiverBind, implType) match {
        case (Some(bindCall), Some(impl)) if bindCall.name == "bind" =>
          classLiteralType(bindCall.argument.argumentIndex(1).headOption).foreach { iface =>
            addBinding(key = iface, impl = impl)
          }
        case _ =>
      }

    case "toInstance" =>
      val receiverBind = call.receiver.collectAll[Call].headOption
      val instanceType = call.argument.argumentIndex(1).headOption.flatMap(allocationTypeOf)
      (receiverBind, instanceType) match {
        case (Some(bindCall), Some(concreteType)) if bindCall.name == "bind" =>
          classLiteralType(bindCall.argument.argumentIndex(1).headOption).foreach { ifaceType =>
            addBinding(key = ifaceType, impl = concreteType)
          }
        case _ =>
      }

    case _ =>
  }

  // -------------------------------------------------------------------------
  // @Provides factory methods
  // -------------------------------------------------------------------------

  private def processProvidesMethod(method: Method): Unit = {
    val returnType = Option(method.methodReturn.typeFullName).filter(_.nonEmpty)
    returnType.foreach { rt =>
      method.ast.isReturn.foreach { r =>
        r.astChildren.foreach { child =>
          allocationTypeOf(child).foreach { concrete =>
            addBinding(key = rt, impl = concrete)
          }
        }
      }
    }
  }

  // -------------------------------------------------------------------------
  // Type predicates
  // -------------------------------------------------------------------------

  private def isGuiceModuleType(fullName: String): Boolean =
    fullName == "com.google.inject.AbstractModule" ||
      fullName == "com.google.inject.PrivateModule" ||
      fullName.startsWith("com.google.inject.")

  private val PROVIDES_ANNOT: Seq[String] = Seq(
    "com.google.inject.Provides",
    "dagger.Provides"
  )
}
