# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

### Requirements

Running Shapes2Shapes requires [SBT](https://www.scala-sbt.org/), the Scala build tool.

### Setup (During Development)

During development, the build is set up to use a local version of the required library [Shar](https://github.com/pseifer/shar), due to incompatibility of certain development tools with SBT's git install of libraries.
To setup Shapes2Shapes without manually installing this library (which also works: To this end, simply clone Shar and run `sbt publishLocal`), update the file `build.sbt` by commenting *in* (removing `//`) the line

```// .dependsOn(RootProject(uri("https://github.com/pseifer/shar.git")))```

and commenting *out* (adding `//` in front of) the line

```libraryDependencies += "de.pseifer" %% "shar" % "0.1.0-SNAPSHOT",```

Then, simply run `sbt` and tasks `run` or `test` as usual.

### Usage

Consider the file [Main.scala](src/main/scala/org.softlang.s2s/Main.scala).
It demonstrates a basic example of using S2S for inferring result shapes and printing a full debug log to standard output.
The `Configuration` provides various settings (see constructor for documentation).

Input are a SCCQ query using standard SPARQL syntax, shapes use standard DL syntax.
The default prefix `:` is configured for examples, and required for queries and shapes.
(The prefix is removed in (formal) output, as well as the internal prefix used for variables; there is a setting for disabling that).
Any additional prefix can be defined via SPARQL prefix declarations.
Currently, automatic renaming between Template and Pattern is not supported.
To this end, the implementation rejects queries, where common concept and property names are used.
