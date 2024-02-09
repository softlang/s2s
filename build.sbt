import scala.sys.process._
import sbt._
import NativePackagerHelper._

val scala3Version = "3.3.1"

// The Shar framework for reasoning as a Git dependeny.
lazy val shar = RootProject(uri("https://github.com/pseifer/shar.git"))

// Native Packager plugin.
enablePlugins(JavaAppPackaging)

lazy val root = project
  .in(file("."))
  .dependsOn(shar)
  .settings(
    // Project metadata.
    name := "s2s",
    maintainer := "pseifer@uni-koblenz.de",
    organization := "org.softlang",
    organizationName := "Softlang, University of Koblenz",
    version := "1.0.0",
    // Project settings.
    run / fork := true,
    run / outputStrategy := Some(StdoutOutput),
    run / javaOptions += "-Xmx4G",
    run / javaOptions += "-Dfile.encoding=UTF-8",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    // Settings for native packer.
    Compile / mainClass := Some("org.softlang.s2s.s2s"),
    Compile / discoveredMainClasses := Seq(),
    Universal / mappings ++= directory("docs"),
    Universal / mappings += file("README.md") -> "README.md",
    Universal / mappings += file("s2s") -> "s2s",
    // Dependencies.
    // Development dependency, local only - install manually and comment out 'dependsOn(shar)'.
    // Note: This is only needed so metals works correctly with the GitHub dependency for SHAR.
    //libraryDependencies += "de.pseifer" %% "shar" % "0.1.0-SNAPSHOT",
    // Dependencies.
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-api" % "5.1.20",
    // JFact reasoner support.
    libraryDependencies += "net.sourceforge.owlapi" % "jfact" % "5.0.3",
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.6",
    // Openllet reasoner support.
    libraryDependencies += "com.github.galigator.openllet" % "openllet-owlapi" % "2.6.5",
    // JSON-LD and JSON (Akarta).
    libraryDependencies += "com.apicatalog" % "titanium-json-ld" % "1.3.1",
    libraryDependencies += "org.glassfish" % "jakarta.json" % "2.0.1",
    // CLI application.
    libraryDependencies += "org.rogach" %% "scallop" % "4.1.0",
    // Parser combinator.
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    // Testing dependencies.
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
