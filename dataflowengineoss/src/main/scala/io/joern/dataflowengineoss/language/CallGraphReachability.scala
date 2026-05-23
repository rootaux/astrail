package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.queryengine.Engine
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Determines function-level reachability via BFS over the call graph. Unlike [[ExtendedCfgNode.reachableByFlows]],
  * this does not track tainted variables through data flow edges. It simply checks whether a source method can reach
  * a sink method or call node by traversing outgoing calls from each method.
  */
object CallGraphReachability {

  /** Internal BFS engine that traverses the call graph forward from source methods. Calls the provided `onVisit`
    * callback for each method visited. Expansion uses both `Engine.methodsForCall` (pointer-analysis-backed) and
    * falls back to method full-name matching for calls that have no resolved CALL edges.
    */
  private def bfs(
    sources: List[Method],
    maxDepth: Int,
    onVisit: (Method, Int) => Unit
  ): Unit = {
    val visited = mutable.Set[Long]()
    val queue   = mutable.Queue[(Method, Int)]()

    for (src <- sources if !visited.contains(src.id)) {
      visited.add(src.id)
      queue.enqueue((src, 0))
    }

    while (queue.nonEmpty) {
      val (currentMethod, depth) = queue.dequeue()
      onVisit(currentMethod, depth)

      if (maxDepth < 0 || depth < maxDepth) {
        val callsInMethod = currentMethod._callViaContainsOut
        for (call <- callsInMethod) {
          val callees = resolveCallees(call)
          for (callee <- callees if !visited.contains(callee.id)) {
            visited.add(callee.id)
            queue.enqueue((callee, depth + 1))
          }
        }
      }
    }
  }

  /** BFS with path tracking. */
  private def bfsWithPath(
    sources: List[Method],
    maxDepth: Int,
    onVisit: (Method, List[Method], Int) => Unit
  ): Unit = {
    val visited = mutable.Set[Long]()
    val queue   = mutable.Queue[(Method, List[Method], Int)]()

    for (src <- sources if !visited.contains(src.id)) {
      visited.add(src.id)
      queue.enqueue((src, List(src), 0))
    }

    while (queue.nonEmpty) {
      val (currentMethod, path, depth) = queue.dequeue()
      onVisit(currentMethod, path, depth)

      if (maxDepth < 0 || depth < maxDepth) {
        val callsInMethod = currentMethod._callViaContainsOut
        for (call <- callsInMethod) {
          val callees = resolveCallees(call)
          for (callee <- callees if !visited.contains(callee.id)) {
            visited.add(callee.id)
            queue.enqueue((callee, path :+ callee, depth + 1))
          }
        }
      }
    }
  }

  private def resolveCallees(call: Call): List[Method] =
    Engine.methodsForCall(call)

  /** Check whether any of the given source methods can reach any of the given sink call nodes through the call graph.
    *
    * @param sources
    *   the methods from which reachability is checked (BFS starting points)
    * @param sinks
    *   the call nodes that represent the target (BFS termination condition)
    * @param maxDepth
    *   optional maximum BFS depth to limit traversal (default: no limit)
    * @return
    *   the subset of sink [[Call]] nodes that are reachable from at least one source method
    */
  def reachableSinks(sources: List[Method], sinks: List[Call], maxDepth: Int = -1): List[Call] = {
    if (sources.isEmpty || sinks.isEmpty) return List.empty

    val sinkSet = sinks.toSet
    val methodToSinkCalls: Map[Long, Set[Call]] = sinks.groupBy(_.method.id).view.mapValues(_.toSet).toMap
    val reachedSinks = mutable.Set[Call]()

    bfs(sources, maxDepth, (method, _) => {
      methodToSinkCalls.get(method.id).foreach(reachedSinks ++= _)
    })

    reachedSinks.toList
  }

  /** Check whether any source method can reach any sink call node through the call graph.
    */
  def isReachable(sources: List[Method], sinks: List[Call], maxDepth: Int = -1): Boolean =
    reachableSinks(sources, sinks, maxDepth).nonEmpty

  /** Returns the reachable sink calls along with the method-level call chain that reaches them.
    */
  def reachableSinksWithCallChain(
    sources: List[Method],
    sinks: List[Call],
    maxDepth: Int = -1
  ): List[(List[Method], Call)] = {
    if (sources.isEmpty || sinks.isEmpty) return List.empty

    val methodToSinkCalls: Map[Long, Set[Call]] = sinks.groupBy(_.method.id).view.mapValues(_.toSet).toMap
    val results = mutable.ListBuffer[(List[Method], Call)]()

    bfsWithPath(sources, maxDepth, (method, path, _) => {
      methodToSinkCalls.get(method.id).foreach { sinksInMethod =>
        sinksInMethod.foreach(sink => results += ((path, sink)))
      }
    })

    results.toList
  }

  /** Check whether any of the given source methods can reach any of the given sink methods through the call graph.
    *
    * @param sources
    *   the methods from which reachability is checked (BFS starting points)
    * @param sinkMethods
    *   the target methods to reach
    * @param maxDepth
    *   optional maximum BFS depth to limit traversal (default: no limit)
    * @return
    *   the subset of sink [[Method]] nodes that are reachable from at least one source method
    */
  def reachableMethods(sources: List[Method], sinkMethods: List[Method], maxDepth: Int = -1): List[Method] = {
    if (sources.isEmpty || sinkMethods.isEmpty) return List.empty

    val sinkIds = sinkMethods.map(_.id).toSet
    val sinkById = sinkMethods.map(m => m.id -> m).toMap
    val reached = mutable.Set[Long]()

    bfs(sources, maxDepth, (method, _) => {
      if (sinkIds.contains(method.id)) reached.add(method.id)
    })

    reached.flatMap(sinkById.get).toList
  }

  /** Check whether any source method can reach any sink method through the call graph.
    */
  def isMethodReachable(sources: List[Method], sinkMethods: List[Method], maxDepth: Int = -1): Boolean =
    reachableMethods(sources, sinkMethods, maxDepth).nonEmpty

  /** Returns the reachable sink methods along with the method-level call chain from source to sink.
    *
    * @return
    *   list of method chains where the first element is a source and the last is a reached sink
    */
  def reachableMethodsWithCallChain(
    sources: List[Method],
    sinkMethods: List[Method],
    maxDepth: Int = -1
  ): List[List[Method]] = {
    if (sources.isEmpty || sinkMethods.isEmpty) return List.empty

    val sinkIds = sinkMethods.map(_.id).toSet
    val results = mutable.ListBuffer[List[Method]]()

    bfsWithPath(sources, maxDepth, (method, path, _) => {
      if (sinkIds.contains(method.id)) results += path
    })

    results.toList
  }

}
