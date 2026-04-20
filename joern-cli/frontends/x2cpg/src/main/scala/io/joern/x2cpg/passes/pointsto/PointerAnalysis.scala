package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.ICallResolver
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/** Runs Andersen-style pointer analysis after [[io.joern.x2cpg.passes.callgraph.DynamicCallLinker]] and refines
  * virtual-dispatch CALL edges using the computed receiver types. Falls back to CHA edges for unresolved receivers.
  */
class PointerAnalysis(cpg: Cpg) extends CpgPass(cpg) {

  import PointerAnalysis.logger

  /** Resolved virtual-dispatch targets keyed by call node id. Populated during [[run]]; empty before. */
  @volatile var lastResolvedTargets: Map[Long, Set[String]] = Map.empty

  /** Points-to map from the last solver run (variable → set of allocation-site indices). */
  @volatile var lastPointsTo: Map[String, PointsToSet] = Map.empty

  /** Allocation site table from the last solver run. Kept so consumers can map indices back to types. */
  @volatile var lastAllocTable: Option[AllocationSiteTable] = None

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val passStart = System.nanoTime()

    val dynamicCalls = cpg.call.filter(_.dispatchType == DispatchTypes.DYNAMIC_DISPATCH).l
    if (dynamicCalls.isEmpty) {
      logger.info("PointerAnalysis: no dynamic dispatch calls found, skipping")
      return
    }
    val totalDynamicCalls = dynamicCalls.size

    // DI binding collection
    val diStart    = System.nanoTime()
    val diBindings = new DependencyInjectionCollector(cpg).collect()
    val diMs       = (System.nanoTime() - diStart) / 1e6

    // Constraint collection
    val collectStart = System.nanoTime()
    val collector    = new ConstraintCollector(cpg, diBindings)
    val constraints  = collector.collect()
    val collectMs    = (System.nanoTime() - collectStart) / 1e6

    val totalMethods     = constraints.size
    val totalConstraints = constraints.valuesIterator.map(_.size).sum
    val allocSiteCount   = collector.allocTable.size

    // Andersen Solver
    val solveStart = System.nanoTime()
    val solver     = new AndersenSolver(cpg, collector.allocTable, constraints)
    val ptMap      = solver.solve()
    val solveMs    = (System.nanoTime() - solveStart) / 1e6

    lastResolvedTargets = solver.resolvedCallTargets.view
      .collect { case (id, targets) if targets.nonEmpty => id -> targets.toSet }
      .toMap
    lastPointsTo   = ptMap
    lastAllocTable = Some(collector.allocTable)

    val resolvedCallSites   = lastResolvedTargets.size
    val unresolvedCallSites = solver.resolvedCallTargets.size - resolvedCallSites
    val chaFallbackCalls    = totalDynamicCalls - resolvedCallSites

    val resolver = new PointerAnalysisCallResolver(cpg, lastResolvedTargets)
    PointerAnalysis.register(cpg.graph, resolver)

    // Edge rewriting
    val edgeStart        = System.nanoTime()
    val methodByFullName = cpg.method.toList.map(m => m.fullName -> m).toMap
    var edgesAdded       = 0
    var callsRefined     = 0

    dynamicCalls.foreach { call =>
      val preciseTargets: Set[String] =
        solver.resolvedCallTargets.get(call.id()).map(_.toSet).getOrElse(Set.empty)

      if (preciseTargets.nonEmpty) {
        callsRefined += 1
        edgesAdded += refineCallEdges(call, preciseTargets, methodByFullName, dstGraph)
      }
    }
    val edgeMs = (System.nanoTime() - edgeStart) / 1e6

    val totalMs = (System.nanoTime() - passStart) / 1e6

    // Metrics summary
    val summary =
      s"""[+] PointerAnalysis completed in ${f"$totalMs%.0f"}ms
         |  DI bindings:    ${diBindings.interfaceToImpls.size} interface bindings, ${diBindings.classRegistrations.size} class registrations (${f"$diMs%.0f"}ms)
         |  Constraints:    $totalConstraints across $totalMethods methods, $allocSiteCount allocation sites (${f"$collectMs%.0f"}ms)
         |  Solver:         ${solver.worklistIterations} worklist iterations, ${solver.contextCount} contexts, ${solver.variableCount} variables, ${solver.subsetEdgeCount} subset edges (${f"$solveMs%.0f"}ms)
         |  Call sites:     $totalDynamicCalls dynamic dispatch, $resolvedCallSites resolved by PTA, $chaFallbackCalls CHA fallback, $unresolvedCallSites visited-but-empty
         |  Edges added:    $edgesAdded new CALL edges across $callsRefined refined call sites (${f"$edgeMs%.0f"}ms)""".stripMargin
    System.err.println(summary)
    logger.info(summary)
  }

  /** @return number of new CALL edges added */
  private def refineCallEdges(
    call: Call,
    preciseTargets: Set[String],
    methodByFullName: Map[String, Method],
    dstGraph: DiffGraphBuilder
  ): Int = {
    val existingEdgeTargets: Set[String] =
      call._callOut.collect { case m: Method => m.fullName }.toSet

    var added = 0
    preciseTargets.foreach { fn =>
      if (!existingEdgeTargets.contains(fn)) {
        methodByFullName.get(fn).foreach { target =>
          dstGraph.addEdge(call, target, EdgeTypes.CALL)
          added += 1
        }
      }
    }
    added
  }
}

object PointerAnalysis {
  private val logger: Logger = LoggerFactory.getLogger(classOf[PointerAnalysis])

  /** Registry of completed analyses keyed by flatgraph instance. */
  private val registry = new java.util.concurrent.ConcurrentHashMap[flatgraph.Graph, ICallResolver]()

  def register(graph: flatgraph.Graph, resolver: ICallResolver): Unit =
    registry.put(graph, resolver)

  def unregister(graph: flatgraph.Graph): Unit =
    registry.remove(graph)

  /** Returns the PTA-backed resolver for the given graph, or `None` if no analysis has been run. */
  def resolverFor(graph: flatgraph.Graph): Option[ICallResolver] =
    Option(registry.get(graph))
}
