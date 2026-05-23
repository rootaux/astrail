package io.joern.dataflowengineoss

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.dotextension.DdgNodeDot
import io.joern.dataflowengineoss.language.nodemethods.{ExpressionMethods, ExtendedCfgNodeMethods}
import io.shiftleft.codepropertygraph.generated.help.Doc

import scala.language.implicitConversions

package object language {

  implicit def cfgNodeToMethodsQp[NodeType <: CfgNode](node: NodeType): ExtendedCfgNodeMethods[NodeType] =
    new ExtendedCfgNodeMethods(node)

  implicit def expressionMethods[NodeType <: Expression](node: NodeType): ExpressionMethods[NodeType] =
    new ExpressionMethods(node)

  implicit def toExtendedCfgNode[NodeType <: CfgNode](traversal: IterableOnce[NodeType]): ExtendedCfgNode =
    new ExtendedCfgNode(traversal.iterator)

  implicit def toDdgNodeDot(traversal: IterableOnce[Method]): DdgNodeDot =
    new DdgNodeDot(traversal.iterator)

  implicit def toDdgNodeDotSingle(method: Method): DdgNodeDot =
    new DdgNodeDot(Iterator.single(method))

  implicit def toCallGraphReachability(traversal: IterableOnce[Call]): CallGraphReachabilityExt =
    new CallGraphReachabilityExt(traversal.iterator)

  implicit def toMethodCallGraphReachability(traversal: IterableOnce[Method]): MethodCallGraphReachabilityExt =
    new MethodCallGraphReachabilityExt(traversal.iterator)

  /** Extension methods for call-graph-level reachability on [[Call]] traversals (method-to-call). */
  class CallGraphReachabilityExt(val traversal: Iterator[Call]) extends AnyVal {

    /** Returns the sink calls that are reachable from at least one of the given source methods via the call graph.
      * This is a lightweight BFS over outgoing calls — no taint tracking.
      */
    def reachableByCallGraph(sourceTrav: IterableOnce[Method], maxDepth: Int = -1): List[Call] =
      CallGraphReachability.reachableSinks(sourceTrav.iterator.toList, traversal.toList, maxDepth)

    /** Like [[reachableByCallGraph]] but also returns the method-level call chain for each reached sink. */
    def reachableByCallGraphWithChain(sourceTrav: IterableOnce[Method], maxDepth: Int = -1): List[(List[Method], Call)] =
      CallGraphReachability.reachableSinksWithCallChain(sourceTrav.iterator.toList, traversal.toList, maxDepth)
  }

  /** Extension methods for call-graph-level reachability on [[Method]] traversals (method-to-method). */
  class MethodCallGraphReachabilityExt(val traversal: Iterator[Method]) extends AnyVal {

    /** Returns the sink methods that are reachable from at least one of the given source methods via the call graph. */
    def reachableByCallGraph(sourceTrav: IterableOnce[Method], maxDepth: Int = -1): List[Method] =
      CallGraphReachability.reachableMethods(sourceTrav.iterator.toList, traversal.toList, maxDepth)

    /** Like [[reachableByCallGraph]] but also returns the method-level call chain from source to sink. */
    def reachableByCallGraphWithChain(sourceTrav: IterableOnce[Method], maxDepth: Int = -1): List[List[Method]] =
      CallGraphReachability.reachableMethodsWithCallChain(sourceTrav.iterator.toList, traversal.toList, maxDepth)
  }

  implicit def toExtendedPathsTrav[NodeType <: Path](traversal: IterableOnce[NodeType]): PassesExt =
    new PassesExt(traversal.iterator)

  class PassesExt(traversal: Iterator[Path]) {

    @Doc(info = "Filters in paths that pass though the given paths")
    def passes(trav: Iterator[AstNode] => Iterator[?]): Iterator[Path] = {
      traversal.filter(_.elements.exists(_.start.where(trav).nonEmpty))
    }

    @Doc(info = "Filters out paths that pass though the given paths")
    def passesNot(trav: Iterator[AstNode] => Iterator[?]): Iterator[Path] = {
      traversal.filter(_.elements.forall(_.start.where(trav).isEmpty))
    }

  }

}
