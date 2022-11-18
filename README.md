# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

### Requirements

Building Shapes2Shapes requires the Scala build tool [SBT](https://www.scala-sbt.org/) and Java.

### Building, Setup and Execution

Essentially, building and running Shapes2Shapes can be handled via `sbt` directly, or by building a fat jar.

##### Running Shapes2Shapes via SBT

To Be Updated!

In order to run Shapes2Shapes from SBT, launch SBT via `sbt` and execute `run queryfile shapesfile` where `queryfile` contains a single, valid SCCQ and `shapesfile` contains a set of Simple SHACL shapes, with one shape per line. Examples are available in the `examples` folder. Alternatively, run, e.g., `sbt "run examples/q1.sparql examples/s1.shacl"` directly from the command line. Note, however, that this is rather slow due to the startup time of SBT.

##### Building (and running) Shapes2Shapes as a fat JAR

In order to compile the project to a (fat) JAR, run `sbt assembly` (or `make`). Then, Shapes2Shapes can be executed like a normal JAR file (executed via `java -jar target/s2s.jar <arguments>`) or via the `s2s` script on UNIX systems or `s2s.bat` on Windows.

### Usage

To Be Updated!

Consider the file [Main.scala](src/main/scala/org.softlang.s2s/Main.scala).
It demonstrates a basic example of using S2S for inferring result shapes and printing a full debug log to standard output.
The `Configuration` provides various settings (see constructor for documentation).

Input are a SCCQ query using standard SPARQL syntax, shapes use standard DL syntax.
The default prefix `:` is configured for examples, and required for queries and shapes.
(The prefix is removed in (formal) output, as well as the internal prefix used for variables; there is a setting for disabling that).
Any additional prefix can be defined via SPARQL prefix declarations.
Currently, automatic renaming between Template and Pattern is not supported.
To this end, the implementation rejects queries, where common concept and property names are used.

Execute examples via `sbt` and then `run`.
