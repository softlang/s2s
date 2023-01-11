val scala3Version = "3.2.0"

// Name of fat jar produced via 'assembly' plugin.
val jarName = "s2s.jar"

lazy val root = project
  .in(file("."))
  // Release: DL Reasoning Framework.
  .dependsOn(RootProject(uri("https://github.com/pseifer/shar.git")))
  .settings(
    // Project settings.
    name := "shapes2shapes",
    organization := "org.softlang",
    version := "0.0.1",
    javaOptions += "-dfile.encoding=utf-8",
    scalaVersion := scala3Version,
    // Settings for assembly (fat jar).
    assembly / mainClass := Some("org.softlang.s2s.s2s"),
    assembly / assemblyJarName := jarName,
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", _*) => MergeStrategy.discard
      case _                        => MergeStrategy.first
    },
    // Dependencies.
    // Development: DL Reasoning Framework.
    // libraryDependencies += "de.pseifer" %% "shar" % "0.1.0-SNAPSHOT",
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-api" % "5.1.20",
    // JFact reasoner support.
    libraryDependencies += "net.sourceforge.owlapi" % "jfact" % "5.0.3",
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.6",
    // Openllet reasoner support.
    libraryDependencies += "com.github.galigator.openllet" % "openllet-owlapi" % "2.6.5",
    // JSON-LD
    libraryDependencies += "com.apicatalog" % "titanium-json-ld" % "1.3.1",
    libraryDependencies += "org.glassfish" % "jakarta.json" % "2.0.1",
    // CLI Application.
    libraryDependencies += "org.rogach" %% "scallop" % "4.1.0",
    // Testing
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    // Parsing
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
  )
