package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.x2cpg.passes.pointsto.di.{DropwizardCollector, GuiceCollector, HibernateCollector, SpringCollector, VertxCollector}
import org.slf4j.{Logger, LoggerFactory}

/** Runs all framework-specific DI collectors and merges their results into a single [[DiBindings]] table. */
final class DependencyInjectionCollector(cpg: Cpg) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[DependencyInjectionCollector])

  def collect(): DiBindings = {
    val dropwizard = new DropwizardCollector(cpg).collect()
    val guice      = new GuiceCollector(cpg).collect()
    val spring     = new SpringCollector(cpg).collect()
    val vertx      = new VertxCollector(cpg).collect()
    val hibernate  = new HibernateCollector(cpg).collect()

    val merged = merge(dropwizard, guice, spring, vertx, hibernate)
    if (merged.isEmpty) logger.debug("DI collector: no bindings found across any framework")
    else
      logger.debug(
        s"DI collector: ${merged.interfaceToImpls.size} interface bindings, " +
          s"${merged.classRegistrations.size} class registrations " +
          s"(dropwizard=${dropwizard.interfaceToImpls.size}/${dropwizard.classRegistrations.size}, " +
          s"guice=${guice.interfaceToImpls.size}/${guice.classRegistrations.size}, " +
          s"spring=${spring.interfaceToImpls.size}/${spring.classRegistrations.size}, " +
          s"vertx=${vertx.interfaceToImpls.size}/${vertx.classRegistrations.size}, " +
          s"hibernate=${hibernate.interfaceToImpls.size}/${hibernate.classRegistrations.size})"
      )
    merged
  }

  private def merge(parts: DiBindings*): DiBindings = {
    val combinedImpls = scala.collection.mutable.LinkedHashMap.empty[String, Set[String]]
    val combinedRegs  = scala.collection.mutable.LinkedHashSet.empty[String]

    parts.foreach { part =>
      part.interfaceToImpls.foreach { case (key, impls) =>
        combinedImpls.updateWith(key) {
          case Some(existing) => Some(existing ++ impls)
          case None           => Some(impls)
        }
      }
      combinedRegs ++= part.classRegistrations
    }

    DiBindings(combinedImpls.toMap, combinedRegs.toSet)
  }
}

object DependencyInjectionCollector {

  /** Injection-point annotations for fields, parameters, and constructors. Both Jakarta and legacy javax. */
  val INJECT_ANNOT: Set[String] = Set(
    "jakarta.inject.Inject",
    "javax.inject.Inject",
    "com.google.inject.Inject",
    "com.fasterxml.jackson.annotation.JacksonInject"
  )

  /** Spring injection annotations. */
  val SPRING_INJECT_ANNOT: Set[String] = Set(
    "org.springframework.beans.factory.annotation.Autowired",
    "org.springframework.beans.factory.annotation.Value",
    "org.springframework.beans.factory.annotation.Qualifier"
  )

  /** JAX-RS `@Context` — Jersey injects request-scoped framework objects. */
  val CONTEXT_ANNOT: Set[String] = Set(
    "jakarta.ws.rs.core.Context",
    "javax.ws.rs.core.Context"
  )

  /** All annotations indicating DI-populated slots. */
  val ALL_INJECT_ANNOT: Set[String] = INJECT_ANNOT ++ SPRING_INJECT_ANNOT ++ CONTEXT_ANNOT
}
