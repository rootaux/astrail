package io.joern.console.testing

import io.joern.console.cpgcreation.{CpgGenerator, CpgGeneratorFactory, ImportCode, JavaSrcCpgGenerator}
import io.joern.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, FrontendConfig, InstallConfig}
import io.joern.console.cpgcreation.guessLanguage
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.utils.ProjectRoot

import java.nio.file.{Files, Path, Paths}
import scala.util.Try

object ConsoleFixture {
  def apply[T <: Console[Project]](constructor: String => T = { x =>
    new TestConsole(x)
  })(fun: (T, Path) => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("console") { workspaceDir =>
      FileUtil.usingTemporaryDirectory("console") { codeDir =>
        Files.createDirectory(codeDir / "dir1")
        Files.createDirectory(codeDir / "dir2")

        val fooPath    = (codeDir / "dir1" / "Foo.java")
        val fooContent = "class Foo { public static void main(String[] args) { } }"

        val barPath    = (codeDir / "dir2" / "Bar.java")
        val barContent = "class Bar { int bar(int x) { return x; } }"

        Files.writeString(fooPath, fooContent)
        Files.writeString(barPath, barContent)

        val console = constructor(workspaceDir.toString)
        fun(console, codeDir)
        Try(console.cpgs.foreach(cpg => cpg.close()))
        Try(console.workspace.reset)
      }
    }
  }

}

object TestWorkspaceLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = Project(projectFile, path)
}

class TestConsole(workspaceDir: String) extends Console[Project](TestWorkspaceLoader, Paths.get(workspaceDir)) {
  override def config =
    new ConsoleConfig(install = new InstallConfig(Map("SHIFTLEFT_OCULAR_INSTALL_DIR" -> workspaceDir)))

  override def importCode: ImportCode[Project] = new ImportCode(this) {
    override val generatorFactory = new TestCpgGeneratorFactory(config)

    override def java: SourceBasedFrontend =
      new SourceBasedFrontend("testJavaSrcFrontend", Languages.JAVASRC, "", "java") {
        override def cpgGeneratorForLanguage(
          language: String,
          config: FrontendConfig,
          rootPath: Path,
          args: List[String]
        ): Option[CpgGenerator] = {
          val newConfig = new ConsoleConfig(TestConsole.this.config.install, config.withArgs(args))
          new TestCpgGeneratorFactory(newConfig).forLanguage(language)
        }
      }
  }
}

class TestCpgGeneratorFactory(config: ConsoleConfig) extends CpgGeneratorFactory(config) {
  private def newJavaSrcCpgGenerator(): JavaSrcCpgGenerator = {
    JavaSrcCpgGenerator(
      config.frontend,
      Path.of(ProjectRoot.relativise("joern-cli/frontends/javasrc2cpg/target/universal/stage/bin"))
    )
  }

  override def forCodeAt(inputPath: String): Option[CpgGenerator] = {
    guessLanguage(inputPath) match {
      case Some(Languages.JAVASRC) => Option(newJavaSrcCpgGenerator())
      case _                       => None
    }
  }

  override def forLanguage(language: String): Option[CpgGenerator] = language match {
    case Languages.JAVASRC => Option(newJavaSrcCpgGenerator())
    case _                 => None
  }

}
