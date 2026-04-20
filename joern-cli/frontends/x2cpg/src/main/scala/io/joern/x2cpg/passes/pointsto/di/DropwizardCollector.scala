package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.pointsto.DiBindings
import org.slf4j.LoggerFactory

/** Collects DI bindings from HK2 / Jersey / Dropwizard patterns:
  *
  *   - `AbstractBinder.configure()`: `bind(Impl.class).to(Iface.class)`, `bindAsContract(C.class)`,
  *     `bindFactory(F.class).to(T.class)`
  *   - `Application.run()` / `ResourceConfig.configure()`: `register(Resource.class)` for Jersey resource classes
  *
  * HK2's `bind(impl).to(iface)` has the implementation as the argument to `bind` and the interface as the argument
  * to `to` — this is the inverse of Guice's convention.
  */
final class DropwizardCollector(override protected val cpg: Cpg) extends DiHelpers {

  private val logger = LoggerFactory.getLogger(classOf[DropwizardCollector])

  def collect(): DiBindings = {
    cpg.method.nameExact("configure").foreach(processConfigureMethod)
    collectResourceRegistrations()

    val result = DiBindings(
      interfaceToImpls = bindings.view.mapValues(_.toSet).toMap,
      classRegistrations = classRegistrations.toSet
    )
    if (!result.isEmpty)
      logger.debug(s"Dropwizard collector: ${result.interfaceToImpls.size} bindings, ${result.classRegistrations.size} registrations")
    result
  }

  // -------------------------------------------------------------------------
  // HK2 AbstractBinder.configure()
  // -------------------------------------------------------------------------

  private def processConfigureMethod(method: Method): Unit = {
    method.typeDecl.headOption.foreach { decl =>
      val ancestry = ancestorTypeFullNames(decl)
      if (ancestry.exists(isHk2BinderType)) {
        method.ast.isCall.foreach(processBindingCall)
      }
    }
  }

  private def processBindingCall(call: Call): Unit = call.name match {
    case "to" =>
      // bind(Impl.class).to(Iface.class)
      val receiverBind = call.receiver.collectAll[Call].headOption
        .orElse(call.argument.argumentIndex(0).collectAll[Call].headOption)
      val ifaceType = classLiteralType(call.argument.argumentIndex(1).headOption)
      (receiverBind, ifaceType) match {
        case (Some(bindCall), Some(iface)) if bindCall.name == "bind" =>
          classLiteralType(bindCall.argument.argumentIndex(1).headOption).foreach { impl =>
            addBinding(key = iface, impl = impl)
          }
        case (Some(bindFactory), Some(iface)) if bindFactory.name == "bindFactory" =>
          classLiteralType(bindFactory.argument.argumentIndex(1).headOption).foreach { factoryType =>
            implsFromFactory(factoryType).foreach(c => addBinding(iface, c))
          }
        case _ =>
      }

    case "bindAsContract" =>
      classLiteralType(call.argument.argumentIndex(1).headOption).foreach { t =>
        addBinding(key = t, impl = t)
      }

    case _ =>
  }

  private def implsFromFactory(factoryFullName: String): List[String] = {
    cpg.typeDecl
      .fullNameExact(factoryFullName)
      .method
      .nameExact("provide")
      .ast
      .isReturn
      .flatMap(r => r.astChildren.flatMap(allocationTypeOf))
      .dedup
      .l
  }

  // -------------------------------------------------------------------------
  // Jersey resource registration: register(Resource.class)
  // -------------------------------------------------------------------------

  private def collectResourceRegistrations(): Unit = {
    cpg.method.nameExact("run", "configure", "registerEndpoints").foreach { method =>
      method.typeDecl.headOption.foreach { decl =>
        val ancestry = ancestorTypeFullNames(decl)
        if (ancestry.exists(isDropwizardApplicationType) || ancestry.exists(isJerseyResourceConfigType)) {
          method.ast.isCall.nameExact("register").foreach { call =>
            call.argument
              .argumentIndex(1)
              .headOption
              .flatMap(n => classLiteralType(Some(n)))
              .foreach(classRegistrations.add)
          }
        }
      }
    }
  }

  // -------------------------------------------------------------------------
  // Type predicates
  // -------------------------------------------------------------------------

  private def isHk2BinderType(fullName: String): Boolean =
    fullName == "org.glassfish.hk2.utilities.binding.AbstractBinder" ||
      (fullName.endsWith(".AbstractBinder") && fullName.contains("hk2"))

  private def isDropwizardApplicationType(fullName: String): Boolean =
    fullName == "io.dropwizard.core.Application" ||
      fullName == "io.dropwizard.Application"

  private def isJerseyResourceConfigType(fullName: String): Boolean =
    fullName == "org.glassfish.jersey.server.ResourceConfig" ||
      fullName == "jakarta.ws.rs.core.Application" ||
      fullName == "javax.ws.rs.core.Application"
}
