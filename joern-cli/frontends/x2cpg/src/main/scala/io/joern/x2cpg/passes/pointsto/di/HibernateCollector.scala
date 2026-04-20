package io.joern.x2cpg.passes.pointsto.di

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.pointsto.DiBindings
import org.slf4j.LoggerFactory

/** Collects DI bindings from Hibernate / JPA patterns: well-known interface→impl mappings (EntityManager,
  * SessionFactory, etc.), DAO classes with `@PersistenceContext` fields, reflectively-instantiated types
  * (UserType, AttributeConverter, Interceptor), and entity event listeners.
  */
final class HibernateCollector(override protected val cpg: Cpg) extends DiHelpers {

  private val logger = LoggerFactory.getLogger(classOf[HibernateCollector])

  def collect(): DiBindings = {
    addWellKnownJpaBindings()
    collectDaoClasses()
    collectReflectivelyInstantiatedTypes()
    collectEntityListeners()

    val result = DiBindings(
      interfaceToImpls = bindings.view.mapValues(_.toSet).toMap,
      classRegistrations = classRegistrations.toSet
    )
    if (!result.isEmpty)
      logger.debug(
        s"Hibernate collector: ${result.interfaceToImpls.size} bindings, ${result.classRegistrations.size} registrations"
      )
    result
  }

  // -------------------------------------------------------------------------
  // Well-known JPA/Hibernate interface → impl bindings
  // -------------------------------------------------------------------------

  /** Seed well-known JPA interface → Hibernate implementation bindings. */
  private def addWellKnownJpaBindings(): Unit = {
    // EntityManager → Hibernate SessionImpl
    ENTITY_MANAGER_IFACES.foreach { iface =>
      ENTITY_MANAGER_IMPLS.foreach { impl =>
        addBinding(iface, impl)
      }
    }

    // SessionFactory → Hibernate SessionFactoryImpl
    SESSION_FACTORY_IFACES.foreach { iface =>
      SESSION_FACTORY_IMPLS.foreach { impl =>
        addBinding(iface, impl)
      }
    }

    // Session → Hibernate SessionImpl
    SESSION_IFACES.foreach { iface =>
      SESSION_IMPLS.foreach { impl =>
        addBinding(iface, impl)
      }
    }

    // EntityTransaction → Hibernate TransactionImpl
    TRANSACTION_IFACES.foreach { iface =>
      TRANSACTION_IMPLS.foreach { impl =>
        addBinding(iface, impl)
      }
    }
  }

  // -------------------------------------------------------------------------
  // DAO classes with @PersistenceContext
  // -------------------------------------------------------------------------

  /** Register DAO classes holding `@PersistenceContext` fields (supplements SpringCollector). */
  private def collectDaoClasses(): Unit = {
    cpg.typeDecl.foreach { decl =>
      val hasPersistenceContext = decl.member.exists { m =>
        m.astChildren.collectAll[Annotation].fullName.exists(PERSISTENCE_CONTEXT_ANNOT.contains)
      }
      if (hasPersistenceContext) {
        classRegistrations.add(decl.fullName)
        // Bind the DAO to every interface it implements.
        decl.inheritsFromTypeFullName.foreach { parentType =>
          if (parentType != decl.fullName && parentType != "java.lang.Object") {
            addBinding(parentType, decl.fullName)
          }
        }
      }
    }
  }

  // -------------------------------------------------------------------------
  // Reflectively instantiated Hibernate types
  // -------------------------------------------------------------------------

  /** Register UserType, AttributeConverter, Interceptor, etc. (reflectively instantiated by Hibernate). */
  private def collectReflectivelyInstantiatedTypes(): Unit = {
    cpg.typeDecl.foreach { decl =>
      val ancestry = ancestorTypeFullNames(decl)
      if (ancestry.exists(REFLECTIVE_HIBERNATE_TYPES.contains)) {
        classRegistrations.add(decl.fullName)
      }
    }
  }

  // -------------------------------------------------------------------------
  // Entity listeners
  // -------------------------------------------------------------------------

  /** Register `@EntityListeners` targets and Hibernate event listener implementations. */
  private def collectEntityListeners(): Unit = {
    // @EntityListeners annotation on entity classes — extract class literal arguments
    cpg.typeDecl.foreach { decl =>
      decl.annotation.fullNameExact(ENTITY_LISTENERS_ANNOT*).foreach { annot =>
        annot.astChildren.foreach { child =>
          classLiteralType(Some(child)).foreach(classRegistrations.add)
        }
      }
    }

    // Classes implementing Hibernate event listener interfaces
    cpg.typeDecl.foreach { decl =>
      val ancestry = ancestorTypeFullNames(decl)
      if (ancestry.exists(isHibernateEventListenerType)) {
        classRegistrations.add(decl.fullName)
      }
    }
  }

  // -------------------------------------------------------------------------
  // Constants
  // -------------------------------------------------------------------------

  private val ENTITY_MANAGER_IFACES = Set(
    "jakarta.persistence.EntityManager",
    "javax.persistence.EntityManager"
  )
  private val ENTITY_MANAGER_IMPLS = Set(
    "org.hibernate.internal.SessionImpl",
    "org.hibernate.jpa.internal.EntityManagerImpl"
  )

  private val SESSION_FACTORY_IFACES = Set(
    "org.hibernate.SessionFactory",
    "jakarta.persistence.EntityManagerFactory",
    "javax.persistence.EntityManagerFactory"
  )
  private val SESSION_FACTORY_IMPLS = Set(
    "org.hibernate.internal.SessionFactoryImpl"
  )

  private val SESSION_IFACES = Set(
    "org.hibernate.Session"
  )
  private val SESSION_IMPLS = Set(
    "org.hibernate.internal.SessionImpl"
  )

  private val TRANSACTION_IFACES = Set(
    "jakarta.persistence.EntityTransaction",
    "javax.persistence.EntityTransaction",
    "org.hibernate.Transaction"
  )
  private val TRANSACTION_IMPLS = Set(
    "org.hibernate.engine.transaction.internal.TransactionImpl"
  )

  private val PERSISTENCE_CONTEXT_ANNOT = Set(
    "jakarta.persistence.PersistenceContext",
    "javax.persistence.PersistenceContext"
  )

  private val ENTITY_LISTENERS_ANNOT = Seq(
    "jakarta.persistence.EntityListeners",
    "javax.persistence.EntityListeners"
  )

  /** Hibernate types that are instantiated reflectively by the framework. */
  private val REFLECTIVE_HIBERNATE_TYPES = Set(
    "org.hibernate.usertype.UserType",
    "org.hibernate.usertype.CompositeUserType",
    "jakarta.persistence.AttributeConverter",
    "javax.persistence.AttributeConverter",
    "org.hibernate.Interceptor",
    "org.hibernate.EmptyInterceptor",
    "org.hibernate.id.IdentifierGenerator",
    "org.hibernate.boot.spi.MetadataContributor",
    "org.hibernate.integrator.spi.Integrator"
  )

  private def isHibernateEventListenerType(fullName: String): Boolean =
    fullName.startsWith("org.hibernate.event.spi.") && fullName.endsWith("EventListener")
}
