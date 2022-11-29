package org.softlang.s2s

import org.rogach.scallop._
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.Log

import scala.util.Failure
import scala.util.Try

/** Command line interface definition. */
class Conf(baseConfiguration: Configuration, 
           arguments: Seq[String]) extends ScallopConf(arguments):
  version(
    "Shapes2Shapes 0.0.1 - Philipp Seifer @ Softlang, University of Koblenz"
  )
  banner("\n" + """Usage: s2s [OPTIONS] <query-file> <shapes-file?>
           |
           | where OPTIONS include:
           |""".stripMargin)
  footer(
    "\n" +
      """For more information about the algorithm, see the full paper:
      |  Philipp Seifer, Daniel Hernandez, Ralf Lämmel and Steffen Staab:
      |  FromFrom Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines
      |  ...TBD...
      |and the repository at
      |  ...URL...
      |""".stripMargin
  )

  val optimize =
    toggle(
      default = Some(true),
      descrYes = "Remove output shapes entailed by others (def: On)"
    )

  val rename =
    toggle(
      default = Some(false),
      descrYes = "Create namespace for input shapes (def: Off)"
    )

  val log =
    toggle(
      default = Some(true),
      descrYes = "Print input and output as a log (def: On)"
    )

  val hidecolon =
    toggle(
      default = Some(true),
      descrYes = "Hide colon in prefixes in log (def: On)"
    )

  val debug =
    toggle(
      default = Some(false),
      descrYes = "Print inferred axioms in the log (def: Off)"
    )

  val prettyVars =
    toggle(
      default = Some(true),
      descrYes = "Pretty variable concepts in log (def: On)"
    )

  val output =
    toggle(
      default = Some(false),
      descrYes = "Print shapes as output (def: Off)"
    )

  val prefix =
    opt[String](
      required = false,
      default = Some(":"),
      descr = "Standard prefix to use (def ':')"
    )

  val renameToken =
    opt[String](
      required = false,
      default = Some("'"),
      descr = "For auto-rename, use this string (def: ')"
    )

  val queryFile = trailArg[String](descr = "File containing input query")

  val shapesFile = trailArg[String](
    required = false,
    descr = "File containing the input set of shapes",
    default = Some("")
  )

  verify()

  /** Convert to a S2S configuration. */
  def toConfiguration: Configuration = baseConfiguration.copy(
    optimizeCandidates = optimize(),
    autoRename = rename(),
    renameToken = renameToken(),
    prefix = prefix(),
    log = log(),
    debug = debug(),
    hidecolon = hidecolon(),
    prettyVariableConcepts = prettyVars(),
    printOutput = output()
  )

/** Shapes2Shapes application entry point. */
@main def s2s(args: String*): Unit =

  // Initialize CLI configuration.
  val conf = Conf(Configuration.mappingOnly, args)

  // Create buffered sources and check for errors.
  val qft = Try(io.Source.fromFile(conf.queryFile()).getLines.mkString("\n"))
  val sft = Try(
    if conf.shapesFile().isEmpty then Set()
    else io.Source.fromFile(conf.shapesFile()).getLines.filter(_.nonEmpty).toSet
  )

  // Output (system) errors, if files can not be opened.
  def handle[T](t: Try[T]): Unit =
    t match
      case Failure(e) => println("FAILURE " + e.getLocalizedMessage)
      case _          => ()

  handle(qft)
  handle(sft)

  // Run Shapes2Shapes with default config and input query/shapes.
  for
    q <- qft
    s <- sft
  do Shapes2Shapes(conf.toConfiguration).run(q, s)

/** Compare two configurations of the algorithm on structured test cases. */
@main def compare(): Unit =

  // Common settings for both Configurations.
  val common = Configuration.default.copy(
    // ...
  )

  val compare = ConfigurationComparison(
    // Configuration 1:
    common.copy(
      cwaForPattern = false
    ),
    // Configuration 2:
    common.copy(
      cwaForPattern = true
    ),
    // Compare S_out for any differences.
    compareResults = true,
    // Compare all subsumptions between query variables as well.
    compareVariableSubsumptions = true,
    // 1000 steps per trial.
    stepTrials = 1000,
    // No random trials.
    randomSets = 0,
    // Generate multiple results.
    stopAfterFirstResult = false
  )

  compare.structured()
