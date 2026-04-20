import sbt.*
import sbt.Keys.*

object Projects {
  val frontendsRoot = file("joern-cli/frontends")

  lazy val joerncli          = project.in(file("joern-cli"))
  lazy val console           = project.in(file("console"))
  lazy val dataflowengineoss = project.in(file("dataflowengineoss"))
  lazy val macros            = project.in(file("macros"))
  lazy val semanticcpg       = project.in(file("semanticcpg"))

  lazy val x2cpg       = project.in(frontendsRoot / "x2cpg")
  lazy val javasrc2cpg = project.in(frontendsRoot / "javasrc2cpg")
  lazy val jimple2cpg  = project.in(frontendsRoot / "jimple2cpg")

  lazy val linterRules = project.in(file("linter-rules"))

}
