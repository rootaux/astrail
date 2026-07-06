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

  "field value read in value position through a polymorphic base" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class Base { Greeter g; }
        |class Derived extends Base { }
        |class App {
        |  static void consume(Greeter x) { x.greet(); }
        |  void run() {
        |    Base b = new Derived();
        |    b.g = new RealGreeter();
        |    consume(b.g);
        |  }
        |}
        |""".stripMargin)

    "read the field back through the concrete-type slot the write used, not the declared-type slot" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "overloaded method resolved by exact signature" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class Registry {
        |  Greeter pick(Greeter g) { return g; }
        |  Greeter pick(int i) { return new OtherGreeter(); }
        |}
        |class App {
        |  void run() {
        |    Registry r = new Registry();
        |    Greeter g = r.pick(new RealGreeter());
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "route the call to the exact overload, not an arbitrary one" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "value read from a static field (singleton)" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class Holder {
        |  static Greeter INSTANCE = new RealGreeter();
        |}
        |class App {
        |  void run() {
        |    Greeter g = Holder.INSTANCE;
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "resolve a dispatch on a value read from a static field" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "collection injection of all beans of a type" should {
    lazy val cpg = code("""
        |import java.util.List;
        |import javax.inject.Inject;
        |import org.springframework.stereotype.Component;
        |interface Handler { void handle(); }
        |@Component class RealHandler implements Handler { public void handle() {} }
        |@Component class OtherHandler implements Handler { public void handle() {} }
        |class App {
        |  @Inject List<Handler> handlers;
        |  void run() {
        |    Handler h = handlers.get(0);
        |    h.handle();
        |  }
        |}
        |class Main {
        |  void go() { new App().run(); }
        |}
        |""".stripMargin)

    "seed the injected collection's elements and resolve a dispatch on a read element" in {
      val targets = ptaTargets(cpg, "handle")
      targets.exists(_.startsWith("RealHandler.handle")) shouldBe true
      targets.exists(_.startsWith("OtherHandler.handle")) shouldBe true
    }
  }

  "implicit single-constructor injection (Spring 4.3+, no @Autowired)" should {
    lazy val cpg = code("""
        |import org.springframework.stereotype.Component;
        |interface Greeter { String greet(); }
        |@Component class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |@Component class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |@Component class Consumer {
        |  Consumer(Greeter g) { g.greet(); }
        |}
        |""".stripMargin)

    "autowire the sole constructor's parameters and resolve a dispatch on them" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe true
    }
  }

  "@Qualifier-pinned injection of one of several impls" should {
    lazy val cpg = code("""
        |import org.springframework.stereotype.Component;
        |import org.springframework.beans.factory.annotation.Autowired;
        |import org.springframework.beans.factory.annotation.Qualifier;
        |interface Greeter { String greet(); }
        |@Component class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |@Component class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  @Autowired @Qualifier("realGreeter") Greeter g;
        |  void run() { g.greet(); }
        |}
        |class Main { void go() { new App().run(); } }
        |""".stripMargin)

    "inject only the qualified bean, not every impl of the interface" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "Spring FactoryBean produces its bean via getObject()" should {
    lazy val cpg = code("""
        |import org.springframework.stereotype.Component;
        |import org.springframework.beans.factory.FactoryBean;
        |import org.springframework.beans.factory.annotation.Autowired;
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |@Component class GreeterFactory implements FactoryBean<Greeter> {
        |  public Greeter getObject() { return new RealGreeter(); }
        |  public Class<?> getObjectType() { return Greeter.class; }
        |}
        |class App {
        |  @Autowired Greeter g;
        |  void run() { g.greet(); }
        |}
        |class Main { void go() { new App().run(); } }
        |""".stripMargin)

    "bind the bean type to the concrete type getObject() returns" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "reflective Class.forName(name).newInstance()" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  void run() throws Exception {
        |    Object o = Class.forName("RealGreeter").newInstance();
        |    Greeter g = (Greeter) o;
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "allocate the named type and resolve a dispatch through the cast" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "reflective Class.forName(name).getDeclaredConstructor().newInstance()" should {
    lazy val cpg = code("""
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  void run() throws Exception {
        |    Object o = Class.forName("RealGreeter").getDeclaredConstructor().newInstance();
        |    Greeter g = (Greeter) o;
        |    g.greet();
        |  }
        |}
        |""".stripMargin)

    "follow the constructor chain to the named type and resolve the dispatch" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe false
    }
  }

  "ServiceLoader iterates the service interface's implementations" should {
    lazy val cpg = code("""
        |import java.util.ServiceLoader;
        |interface Greeter { String greet(); }
        |class RealGreeter implements Greeter { public String greet() { return "hi"; } }
        |class OtherGreeter implements Greeter { public String greet() { return "yo"; } }
        |class App {
        |  void run() {
        |    ServiceLoader<Greeter> loader = ServiceLoader.load(Greeter.class);
        |    for (Greeter g : loader) { g.greet(); }
        |  }
        |}
        |""".stripMargin)

    "resolve a dispatch on an element to every impl of the loaded service" in {
      val targets = ptaTargets(cpg, "greet")
      targets.exists(_.startsWith("RealGreeter.greet")) shouldBe true
      targets.exists(_.startsWith("OtherGreeter.greet")) shouldBe true
    }
  }

  "a thrown object caught by a catch clause" should {
    lazy val cpg = code("""
        |class MyException extends RuntimeException { void handle() {} }
        |class SubException extends MyException { public void handle() {} }
        |class OtherException extends MyException { public void handle() {} }
        |class App {
        |  void run() {
        |    try { throw new SubException(); }
        |    catch (MyException e) { e.handle(); }
        |  }
        |}
        |""".stripMargin)

    "flow to the catch variable and resolve a dispatch on it to the thrown type" in {
      val targets = ptaTargets(cpg, "handle")
      targets.exists(_.startsWith("SubException.handle")) shouldBe true
      targets.exists(_.startsWith("OtherException.handle")) shouldBe false
    }
  }
}
