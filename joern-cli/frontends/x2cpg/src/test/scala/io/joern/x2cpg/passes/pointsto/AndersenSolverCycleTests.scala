package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for Lazy Cycle Detection in [[AndersenSolver]]. These drive the solver directly with synthetic
  * constraints so the behaviour does not depend on frontend CPG lowering — an empty CPG is enough because copy/alloc
  * constraints need no method/type lookup tables.
  */
class AndersenSolverCycleTests extends AnyWordSpec with Matchers {

  import Constraint.*

  private def sitesOf(result: Map[String, PointsToSet]): Set[Int] =
    result.values.flatMap(_.iterator.toSet).toSet

  "Lazy cycle elimination" should {

    "collapse a copy cycle while keeping the points-to result correct" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val site       = allocTable.intern(1L, "pkg.T") // alloc index 0

      // a ⊇ {site}; b ⊇ a; c ⊇ b; a ⊇ c  → a,b,c form a copy cycle, all aliasing {site}.
      val constraints = Map(
        "M" -> IndexedSeq(
          Alloc("a", site),
          Copy("b", "a"),
          Copy("c", "b"),
          Copy("a", "c")
        )
      )
      val solver = new AndersenSolver(cpg, allocTable, constraints)
      val result = solver.solve()

      // Correctness: the whole cycle points to exactly {site}.
      sitesOf(result) shouldBe Set(site)
      // Cycle elimination fired: the three cyclic variables collapsed to a single representative.
      solver.variableCount shouldBe 1
    }

    "give the same points-to result for an acyclic chain (no collapse needed)" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val site       = allocTable.intern(2L, "pkg.U")

      // a ⊇ {site}; b ⊇ a; c ⊇ b  → acyclic chain, no cycle to collapse.
      val constraints = Map(
        "M" -> IndexedSeq(
          Alloc("a", site),
          Copy("b", "a"),
          Copy("c", "b")
        )
      )
      val solver = new AndersenSolver(cpg, allocTable, constraints)
      val result = solver.solve()

      sitesOf(result) shouldBe Set(site)
      // No cycle: a, b and c stay distinct.
      solver.variableCount shouldBe 3
    }

    "collapse a longer cycle carrying two allocation sites" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val s1         = allocTable.intern(10L, "pkg.A") // 0
      val s2         = allocTable.intern(11L, "pkg.B") // 1

      // Two allocs enter a 4-node copy cycle a→b→c→d→a; both must reach every node.
      val constraints = Map(
        "M" -> IndexedSeq(
          Alloc("a", s1),
          Alloc("c", s2),
          Copy("b", "a"),
          Copy("c", "b"),
          Copy("d", "c"),
          Copy("a", "d")
        )
      )
      val solver = new AndersenSolver(cpg, allocTable, constraints)
      val result = solver.solve()

      sitesOf(result) shouldBe Set(s1, s2)
      solver.variableCount shouldBe 1
    }
  }
}
