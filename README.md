# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

### Requirements

Building Shapes2Shapes requires [SBT](https://www.scala-sbt.org/), the Scala build tool (and Java). To compile the project, simply run `sbt compile`. 

### Running Shapes2Shapes from SBT

In order to run Shapes2Shapes from SBT, launch SBT via `sbt` and execute `run queryfile shapesfile` where `queryfile` contains a single, valid SCCQ and `shapesfile` contains a set of Simple SHACL shapes, with one shape per line. Alternatively, run `sbt "run queryfile shapesfile"` directly from the command line. Note, however, that his is rather slow due to SBT's startup time. 


### Building (and running) a fat jar

In order to compile the project to a (fat) jar, run `sbt assembly`. Then, Shapes2Shapes can be executed like a normal jar file, taking both the query file and shapes file defined in the previous section as its arguments (e.g., `java -jar target/s2s.jar queryfile shapesfile`).

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

Execute examples via `sbt` and then `run`.