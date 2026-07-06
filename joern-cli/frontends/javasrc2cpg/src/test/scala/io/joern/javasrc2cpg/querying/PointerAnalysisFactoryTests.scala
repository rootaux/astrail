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
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "allocation flowing through a cast" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  static Object make() { return new RealGreeter(); }
        |  void run() {
        |    Object o = make();
        |    Greeter g = (Greeter) o;
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "preserve points-to across the cast and resolve the dispatch" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "allocation stored into and loaded from an array element" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  void run() {
        |    Greeter[] gs = new Greeter[1];
        |    gs[0] = new RealGreeter();
        |    Greeter g = gs[0];
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "flow the element through the array's synthetic slot and resolve the dispatch" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "allocation added to and read from a collection" should {
    lazy val cpg = code("""
        |import java.util.ArrayList;
        |import java.util.List;
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  void run() {
        |    List<Greeter> gs = new ArrayList<Greeter>();
        |    gs.add(new RealGreeter());
        |    Greeter g = gs.get(0);
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "flow the element through the collection's synthetic slot and resolve the dispatch" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "lambda assigned to a functional interface" should {
    lazy val cpg = code("""
        |import java.util.function.Supplier;
        |class App {
        |  String make() {
        |    Supplier<String> s = () -> "hi";
        |    return s.get();
        |  }
        |}
        |""".stripMargin)

    "resolve the functional dispatch to the synthetic lambda method" in {
      val targets = ptaTargets(cpg, "get")
      targets.exists(_.contains("lambda")) shouldBe true
    }
  }

  "method reference assigned to a functional interface" should {
    lazy val cpg = code("""
        |import java.util.function.Supplier;
        |class App {
        |  static String helper() { return "hi"; }
        |  String make() {
        |    Supplier<String> s = App::helper;
        |    return s.get();
        |  }
        |}
        |""".stripMargin)

    "resolve the functional dispatch to the referenced method" in {
      val targets = ptaTargets(cpg, "get")
      targets.exists(_.contains("helper")) shouldBe true
    }
  }
}
