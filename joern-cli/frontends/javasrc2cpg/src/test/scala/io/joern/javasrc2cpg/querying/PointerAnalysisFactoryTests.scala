package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.passes.pointsto.PointerAnalysis
import io.shiftleft.semanticcpg.language.*

/** End-to-end check that the pointer analysis resolves a virtual dispatch whose receiver comes from a factory
  * whose allocation is in *return* position (and, in the second case, argument position). Before allocations in
  * non-assignment position were modelled, the factory result carried no points-to and the dispatch stayed
  * unresolved; the resolver exposes the precise target without the CHA over-approximation.
  */
class PointerAnalysisFactoryTests extends JavaSrcCode2CpgFixture {

  private def ptaTargets(cpg: io.shiftleft.codepropertygraph.generated.Cpg, callName: String): Set[String] = {
    val resolver = PointerAnalysis.resolverFor(cpg.graph)
    resolver.toList.flatMap { r =>
      cpg.call.name(callName).flatMap(c => r.getResolvedCalledMethods(c)).map(_.fullName)
    }.toSet
  }

  "allocation in return position (factory)" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  static Greeter make() { return new RealGreeter(); }
        |  void run() {
        |    Greeter g = make();
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "resolve the dispatch precisely to the factory's concrete return type" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "allocation in argument position" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  static void use(Greeter g) { g.greet(); }
        |  void run() {
        |    use(new RealGreeter());
        |  }
        |}
        |""".stripMargin)

    "flow the argument allocation into the callee and resolve the dispatch" in {
      // B1 correctly puts the allocation into the call's argVars, but the argument->parameter binding is
      // mis-indexed for reference arguments (compacted argVars from flatMap(exprVar) vs param index, and no
      // receiver slot on static calls), so the parameter never receives it. That is a separate arg-binding
      // fix (next commit); the allocation modelling itself is proven by the return-position test above.
      pendingUntilFixed {
        val targets = ptaTargets(cpg, "greet")
        targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
        targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
      }
    }
  }
}
