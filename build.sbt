val scala3Version = "3.2.0"

val jarName = "s2s.jar"

lazy val root = project
  .in(file("."))
  // Release
  .dependsOn(RootProject(uri("https://github.com/pseifer/shar.git")))
  .settings(
    name := "shapes2shapes",
    javaOptions += "-Dfile.encoding=UTF-8",
    organization := "org.softlang",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    // Fat jar.
    assembly / assemblyJarName := jarName,
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", _*) => MergeStrategy.discard
      case _                        => MergeStrategy.first
    },
    // Development
    // libraryDependencies += "de.pseifer" %% "shar" % "0.1.0-SNAPSHOT",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
  )
