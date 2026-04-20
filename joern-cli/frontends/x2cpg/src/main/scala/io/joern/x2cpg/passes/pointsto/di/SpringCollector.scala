package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.pointsto.DiBindings
import org.slf4j.LoggerFactory

/** Collects DI bindings from Spring patterns: component scanning (`@Component`, `@Service`, etc.) and
  * `@Bean` factory methods in `@Configuration` classes.
  */
final class SpringCollector(override protected val cpg: Cpg) extends DiHelpers {

  private val logger = LoggerFactory.getLogger(classOf[SpringCollector])

  def collect(): DiBindings = {
    collectComponents()
    collectBeanMethods()

    val result = DiBindings(
      interfaceToImpls = bindings.view.mapValues(_.toSet).toMap,
      classRegistrations = classRegistrations.toSet
    )
    if (!result.isEmpty)
      logger.debug(
        s"Spring collector: ${result.interfaceToImpls.size} bindings, ${result.classRegistrations.size} components"
      )
    result
  }

  // -------------------------------------------------------------------------
  // Component scanning
  // -------------------------------------------------------------------------

  private def collectComponents(): Unit = {
    cpg.typeDecl.foreach { decl =>
      val annots = decl.annotation.fullName.toSet
      if (annots.exists(COMPONENT_ANNOT.contains)) {
        val concreteType = decl.fullName
        classRegistrations.add(concreteType)

        // Every directly-implemented interface becomes a binding key.
        decl.inheritsFromTypeFullName.foreach { parentType =>
          if (parentType != concreteType && parentType != "java.lang.Object") {
            addBinding(key = parentType, impl = concreteType)
          }
        }
      }
    }
  }

  // -------------------------------------------------------------------------
  // @Bean factory methods
  // -------------------------------------------------------------------------

  private def collectBeanMethods(): Unit = {
    cpg.method.where(_.annotation.fullNameExact(BEAN_ANNOT*)).foreach { method =>
      val returnType = Option(method.methodReturn.typeFullName).filter(_.nonEmpty)
      returnType.foreach { rt =>
        // The return type is the binding key.
        var foundAlloc = false
        method.ast.isReturn.foreach { r =>
          r.astChildren.foreach { child =>
            allocationTypeOf(child).foreach { concrete =>
              addBinding(key = rt, impl = concrete)
              foundAlloc = true
            }
          }
        }
        // Even if we couldn't find a concrete allocation, register the return type as a class registration
        // so the constraint collector can still emit a synthetic alloc for it.
        classRegistrations.add(rt)
      }
    }
  }

  // -------------------------------------------------------------------------
  // Annotation constants
  // -------------------------------------------------------------------------

  private val COMPONENT_ANNOT: Set[String] = Set(
    "org.springframework.stereotype.Component",
    "org.springframework.stereotype.Service",
    "org.springframework.stereotype.Repository",
    "org.springframework.stereotype.Controller",
    "org.springframework.web.bind.annotation.RestController",
    "org.springframework.context.annotation.Configuration"
  )

  private val BEAN_ANNOT: Seq[String] = Seq(
    "org.springframework.context.annotation.Bean"
  )
}
