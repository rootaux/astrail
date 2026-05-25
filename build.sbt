name                     := "astrail"
ThisBuild / organization := "io.astrail"
ThisBuild / version      := "0.0.3"
ThisBuild / scalaVersion := "3.7.4"

val cpgVersion = "1.7.62"

lazy val joerncli          = Projects.joerncli
lazy val console           = Projects.console
lazy val dataflowengineoss = Projects.dataflowengineoss
lazy val macros            = Projects.macros
lazy val semanticcpg       = Projects.semanticcpg
lazy val x2cpg             = Projects.x2cpg
lazy val javasrc2cpg       = Projects.javasrc2cpg
lazy val jimple2cpg        = Projects.jimple2cpg
lazy val linterRules       = Projects.linterRules

// aggregate project which doesn't include the helper project `linterRules` - we don't want to include it in any standard task
lazy val root = project
  .in(file("."))
  .aggregate(
    joerncli,
    console,
    dataflowengineoss,
    macros,
    semanticcpg,
    x2cpg,
    javasrc2cpg,
    jimple2cpg
  )
  .dependsOn(linterRules % ScalafixConfig)

ThisBuild / libraryDependencies ++= Seq(
  "org.slf4j"                % "slf4j-api"         % Versions.slf4j,
  "org.apache.logging.log4j" % "log4j-slf4j2-impl" % Versions.log4j % Optional,
  "org.apache.logging.log4j" % "log4j-core"        % Versions.log4j % Optional
  // `Optional` means "not transitive", but still included in "stage/lib"
)

ThisBuild / compile / javacOptions ++= Seq(
  "-g", // debug symbols
  "-Xlint",
  "-proc:none",
  "--release=11"
) ++ {
  // Require Java 13+ due to FileSystems.newFileSystem(Path) API used in project/FileUtils.scala
  val javaVersion = sys.props("java.specification.version").toFloat
  assert(javaVersion.toInt >= 13, s"this build requires JDK13+ - you're using $javaVersion")
  Nil
}

ThisBuild / scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "--release",
  "11",
  "-Xfatal-warnings",
  "-feature",
  "-Wshadow:type-parameter-shadow",
  "-no-indent",
  "-old-syntax",
  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
)

lazy val createDistribution = taskKey[File]("Create a complete Astrail distribution")
createDistribution := {
  val distributionFile = file("target/astrail-cli.zip")
  val zip              = (joerncli / Universal / packageBin).value

  IO.copyFile(zip, distributionFile)

  println(s"created distribution - resulting files: $distributionFile")
  distributionFile
}

ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
  "Atlassian" at "https://packages.atlassian.com/mvn/maven-atlassian-external",
  "Gradle Releases" at "https://repo.gradle.org/gradle/libs-releases/"
)

ThisBuild / Test / fork := true

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

publish / skip := true // don't publish the root project

ThisBuild / Test / packageBin / publishArtifact := true

// trigger an sbt reload when any `application.conf` file changes
Global / checkBuildSources / fileInputs += (baseDirectory.value.toGlob / ** / "resources" / "application.conf")
