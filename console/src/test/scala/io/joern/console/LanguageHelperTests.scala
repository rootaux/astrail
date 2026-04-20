package io.joern.console

import io.shiftleft.codepropertygraph.generated.Languages
import io.joern.console.cpgcreation.{JavaSrcCpgGenerator, guessLanguage}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Paths}

class LanguageHelperTests extends AnyWordSpec with Matchers {

  "LanguageHelper.guessLanguage" should {

    "guess `Java` for .jars/wars/ears" in {
      guessLanguage("foo.jar") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.war") shouldBe Some(Languages.JAVA)
      guessLanguage("foo.ear") shouldBe Some(Languages.JAVA)
    }

    "guess `JavaSrc` for a directory containing `.java`" in {
      FileUtil.usingTemporaryDirectory("oculartests") { tmpDir =>
        val subdir = Files.createDirectory(tmpDir / "subdir")
        (subdir / "ServiceIdentifierComposerVisitorBasedStrategy.java").createWithParentsIfNotExists()
        guessLanguage(tmpDir.toString) shouldBe Some(Languages.JAVASRC)
      }
    }

    "not find anything for an empty directory" in {
      FileUtil.usingTemporaryDirectory("oculartests") { tmpDir =>
        guessLanguage(tmpDir.toString) shouldBe None
      }
    }

  }

  "LanguageHelper.cpgGeneratorForLanguage" should {

    "select JavaSrc frontend for .java source" in {
      val frontend =
        io.joern.console.cpgcreation.cpgGeneratorForLanguage(Languages.JAVASRC, FrontendConfig(), Paths.get("."), Nil)
      frontend.get.isInstanceOf[JavaSrcCpgGenerator] shouldBe true
    }
  }

}
