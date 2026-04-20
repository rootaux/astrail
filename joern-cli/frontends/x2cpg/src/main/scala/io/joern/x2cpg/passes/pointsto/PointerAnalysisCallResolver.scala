package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CallRepr, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.ICallResolver

import scala.collection.mutable

/** [[ICallResolver]] backed by pointer-analysis results. Returns precise targets where available,
  * falls back to existing CHA-derived CALL edges otherwise.
  */
final class PointerAnalysisCallResolver(cpg: Cpg, resolvedTargets: Map[Long, Set[String]]) extends ICallResolver {

  /** Lazy index from method full name to Method node. */
  private lazy val methodByFullName: Map[String, Method] =
    cpg.method.toList.map(m => m.fullName -> m).toMap

  /** Cache of already-computed target lists per call node. */
  private val cache = mutable.HashMap.empty[Long, Iterable[Method]]

  override def triggerCallsiteResolution(callsite: CallRepr): Unit = {
    callsite match {
      case c: Call =>
        resolvedTargets.get(c.id()).foreach { fullNames =>
          val resolved = fullNames.flatMap(methodByFullName.get)
          cache.update(c.id(), resolved)
        }
      case _ =>
    }
  }

  override def triggerMethodCallsiteResolution(method: Method): Unit = {}

  override def getResolvedCalledMethods(callsite: CallRepr): Iterable[Method] = {
    callsite match {
      case c: Call =>
        cache.getOrElse(c.id(), resolvedTargets.get(c.id()) match {
          case Some(fullNames) =>
            val resolved = fullNames.flatMap(methodByFullName.get)
            cache.update(c.id(), resolved)
            resolved
          case None => Iterable.empty
        })
      case _ => Iterable.empty
    }
  }

  override def getResolvedMethodCallsites(method: Method): Iterable[CallRepr] = Iterable.empty

  override def getUnresolvedMethodFullNamesInternal(callsite: CallRepr): Iterable[String] = Iterable.empty

  override def getCalledMethods(callsite: CallRepr): Iterable[Method] = {
    callsite match {
      case c: Call if resolvedTargets.get(c.id()).exists(_.nonEmpty) =>
        getResolvedCalledMethods(callsite)
      case _ =>
        super.getCalledMethods(callsite)
    }
  }
}
