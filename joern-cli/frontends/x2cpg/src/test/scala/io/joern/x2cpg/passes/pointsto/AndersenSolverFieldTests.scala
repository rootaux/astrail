package io.joern.x2cpg.passes.pointsto

import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for incremental (delta) field load/store discharge in [[AndersenSolver]]. Driven directly with
  * synthetic constraints; field slots are resolved from the base's concrete types, so an empty CPG suffices.
  */
class AndersenSolverFieldTests extends AnyWordSpec with Matchers {

  import Constraint.*

  private def pointsTo(result: Map[String, PointsToSet], v: String): Set[Int] =
    result.get(s"-1|$v").map(_.iterator.toSet).getOrElse(Set.empty)

  "Field discharge" should {

    "flow a stored value back through a load on the same field" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val box        = allocTable.intern(1L, "pkg.Box") // 0
      val value      = allocTable.intern(2L, "pkg.Val") // 1

      // base ⊇ {box}; src ⊇ {value}; base.f ⊇ src; dst ⊇ base.f  →  dst ⊇ {value}
      val cs = Map(
        "M" -> IndexedSeq(
          Alloc("base", box),
          Alloc("src", value),
          Store("base", "f", "src"),
          Load("dst", "base", "f")
        )
      )
      val result = new AndersenSolver(cpg, allocTable, cs).solve()
      pointsTo(result, "dst") shouldBe Set(value)
    }

    "wire field slots for a base that grows to two concrete types (delta discharge)" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val boxA       = allocTable.intern(1L, "pkg.BoxA") // 0
      val boxB       = allocTable.intern(2L, "pkg.BoxB") // 1
      val value      = allocTable.intern(3L, "pkg.Val")  // 2

      // base initially ⊇ {boxA:BoxA}; via `other` it also gains {boxB:BoxB}. A store into base.f must reach the
      // field slot of BOTH concrete types, and the load must read both back — exercising the discharge that runs
      // when the base set grows from {BoxA} to {BoxA, BoxB}.
      val cs = Map(
        "M" -> IndexedSeq(
          Alloc("base", boxA),
          Alloc("other", boxB),
          Copy("base", "other"), // base ⊇ other ⊇ {boxB}
          Alloc("src", value),
          Store("base", "f", "src"),
          Load("dst", "base", "f")
        )
      )
      val result = new AndersenSolver(cpg, allocTable, cs).solve()
      pointsTo(result, "dst") shouldBe Set(value)
    }

    "keep distinct field names separate" in {
      val cpg        = Cpg.empty
      val allocTable = new AllocationSiteTable
      val box        = allocTable.intern(1L, "pkg.Box") // 0
      val vf         = allocTable.intern(2L, "pkg.Vf")  // 1
      val vg         = allocTable.intern(3L, "pkg.Vg")  // 2

      // base.f ⊇ {vf}; base.g ⊇ {vg}. A load of f must not pick up g's value.
      val cs = Map(
        "M" -> IndexedSeq(
          Alloc("base", box),
          Alloc("sf", vf),
          Alloc("sg", vg),
          Store("base", "f", "sf"),
          Store("base", "g", "sg"),
          Load("df", "base", "f"),
          Load("dg", "base", "g")
        )
      )
      val result = new AndersenSolver(cpg, allocTable, cs).solve()
      pointsTo(result, "df") shouldBe Set(vf)
      pointsTo(result, "dg") shouldBe Set(vg)
    }
  }
}
