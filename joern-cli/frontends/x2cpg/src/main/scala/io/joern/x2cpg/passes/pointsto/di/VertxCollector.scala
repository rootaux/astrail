package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.pointsto.DiBindings
import org.slf4j.LoggerFactory

/** Collects DI bindings from Vert.x patterns: verticle deployments (class-literal and string-literal),
  * service proxy registrations (`ServiceBinder.register`, `ProxyHelper.registerService`), and Verticle subclass
  * detection for reflective instantiation.
  */
final class VertxCollector(override protected val cpg: Cpg) extends DiHelpers {

  private val logger = LoggerFactory.getLogger(classOf[VertxCollector])

  def collect(): DiBindings = {
    collectVerticleDeployments()
    collectVerticleSubclasses()
    collectServiceProxyRegistrations()

    val result = DiBindings(
      interfaceToImpls = bindings.view.mapValues(_.toSet).toMap,
      classRegistrations = classRegistrations.toSet
    )
    if (!result.isEmpty)
      logger.debug(
        s"Vert.x collector: ${result.interfaceToImpls.size} bindings, ${result.classRegistrations.size} registrations"
      )
    result
  }

  // -------------------------------------------------------------------------
  // Verticle deployment
  // -------------------------------------------------------------------------

  /** Detect `deployVerticle(MyVerticle.class)` and `deployVerticle("com.example.MyVerticle")` calls. */
  private def collectVerticleDeployments(): Unit = {
    cpg.call.nameExact("deployVerticle").foreach { call =>
      call.argument.argumentIndex(1).headOption.foreach { arg =>
        // Class literal: deployVerticle(MyVerticle.class)
        classLiteralType(Some(arg)).foreach(classRegistrations.add)

        // String literal: deployVerticle("com.example.MyVerticle")
        arg match {
          case lit: Literal if looksLikeClassName(lit.code) =>
            val className = lit.code.stripPrefix("\"").stripSuffix("\"")
            classRegistrations.add(className)
          case _ =>
        }
      }
    }
  }

  /** Register concrete subclasses of `AbstractVerticle` / `Verticle` (reflectively instantiated). */
  private def collectVerticleSubclasses(): Unit = {
    cpg.typeDecl.foreach { decl =>
      val ancestry = ancestorTypeFullNames(decl)
      if (ancestry.exists(isVerticleType)) {
        classRegistrations.add(decl.fullName)
      }
    }
  }

  // -------------------------------------------------------------------------
  // Service proxy registration
  // -------------------------------------------------------------------------

  /** Extract interface→impl bindings from `ServiceBinder.register` and `ProxyHelper.registerService`. */
  private def collectServiceProxyRegistrations(): Unit = {
    // ServiceBinder.register(Class<T> iface, T impl)
    cpg.call.nameExact("register").foreach { call =>
      val args = call.argument.l
      if (args.size >= 3) {
        val ifaceArg = classLiteralType(args.lift(1))
        val implArg  = args.lift(2).flatMap(allocationTypeOf)
        (ifaceArg, implArg) match {
          case (Some(iface), Some(impl)) =>
            addBinding(iface, impl)
            classRegistrations.add(impl)
          case _ =>
        }
      }
    }

    // ProxyHelper.registerService(Class<T>, Vertx, T impl, String addr)
    cpg.call.nameExact("registerService").foreach { call =>
      val args = call.argument.l
      if (args.size >= 4) {
        val ifaceArg = classLiteralType(args.lift(1))
        val implArg  = args.lift(3).flatMap(allocationTypeOf)
        (ifaceArg, implArg) match {
          case (Some(iface), Some(impl)) =>
            addBinding(iface, impl)
            classRegistrations.add(impl)
          case _ =>
        }
      }
    }
  }

  // -------------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------------

  private def looksLikeClassName(s: String): Boolean = {
    val stripped = s.stripPrefix("\"").stripSuffix("\"")
    stripped.contains(".") && !stripped.contains(" ") && stripped.matches("[a-zA-Z_][a-zA-Z0-9_.]*")
  }

  private def isVerticleType(fullName: String): Boolean =
    fullName == "io.vertx.core.AbstractVerticle" ||
      fullName == "io.vertx.core.Verticle" ||
      fullName == "io.vertx.core.impl.AbstractVerticle"
}
