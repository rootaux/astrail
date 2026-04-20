package io.joern.console

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Path, Paths, Files}
import scala.collection.mutable
import scala.util.Try

package object cpgcreation {

  /** For a given language, return CPG generator script Note, this doesn't check if the generator is available, that is
    * done in the ImportCode class.
    */
  def cpgGeneratorForLanguage(
    language: String,
    config: FrontendConfig,
    rootPath: Path,
    args: List[String]
  ): Option[CpgGenerator] = {
    lazy val conf = config.withArgs(args)
    language match {
      case Languages.JAVA    => Some(JavaCpgGenerator(conf, rootPath))
      case Languages.JAVASRC => Some(JavaSrcCpgGenerator(conf, rootPath))
      case _                 => None
    }
  }

  /** Heuristically determines language by inspecting file/dir at path.
    */
  def guessLanguage(path: String): Option[String] = {
    val file = Paths.get(path)
    if (Files.isDirectory(file)) {
      guessMajorityLanguageInDir(file)
    } else {
      guessLanguageForRegularFile(file)
    }
  }

  /** Guess the main language for an entire directory (e.g. a whole project), based on a group count of all individual
    * files. Rationale: many projects contain files from different languages, but most often one language is standing
    * out in numbers.
    */
  private def guessMajorityLanguageInDir(directory: Path): Option[String] = {
    assert(Files.isDirectory(directory), s"$directory must be a directory, but wasn't")
    val groupCount = mutable.Map.empty[String, Int].withDefaultValue(0)

    for {
      file <- directory.walk().filterNot(_ == directory)
      if Files.isRegularFile(file)
      guessedLanguage <- guessLanguageForRegularFile(file)
    } {
      val oldValue = groupCount(guessedLanguage)
      groupCount.update(guessedLanguage, oldValue + 1)
    }

    groupCount.toSeq.sortBy(_._2).lastOption.map(_._1)
  }

  private def isJavaBinary(filename: String): Boolean =
    Seq(".jar", ".war", ".ear", ".apk").exists(filename.endsWith)

  private def guessLanguageForRegularFile(file: Path): Option[String] = {
    file.fileName.toLowerCase match {
      case f if isJavaBinary(f)      => Some(Languages.JAVA)
      case f if f.endsWith(".java")  => Some(Languages.JAVASRC)
      case f if f.endsWith(".class") => Some(Languages.JAVA)
      case _                         => None
    }
  }

  def withFileInTmpFile(inputPath: String)(f: Path => Try[String]): Try[String] = {
    FileUtil.usingTemporaryDirectory("cpgcreation") { dir =>
      Paths.get(inputPath).copyToDirectory(dir)
      f(dir)
    }
  }

}
