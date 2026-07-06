package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.x2cpg.passes.pointsto.di.VertxCollector
import io.shiftleft.semanticcpg.language.*

/** Tests for the DI-binding collectors used by the pointer analysis. */
class DiCollectorTests extends JavaSrcCode2CpgFixture {

  "Vert.x service-proxy collector" should {

    "not bind an unrelated non-Vert.x register(...) call" in {
      val cpg = code("""
          |class Registry {
          |  <T> void register(Class<T> iface, T impl) {}
          |}
          |class Service {}
          |class RealService extends Service {}
          |class App {
          |  void setup(Registry reg) {
          |    reg.register(Service.class, new RealService());
          |  }
          |}
          |""".stripMargin)
      val bindings = new VertxCollector(cpg).collect()
      bindings.interfaceToImpls shouldBe empty
    }

    "bind a genuine io.vertx ServiceBinder.register(...) call" in {
      val cpg = code("""
          |import io.vertx.serviceproxy.ServiceBinder;
          |interface MyService {}
          |class MyServiceImpl implements MyService {}
          |class App {
          |  void setup(ServiceBinder binder) {
          |    binder.register(MyService.class, new MyServiceImpl());
          |  }
          |}
          |""".stripMargin)
      val bindings = new VertxCollector(cpg).collect()
      bindings.interfaceToImpls.getOrElse("MyService", Set.empty) should contain("MyServiceImpl")
    }
  }
}
