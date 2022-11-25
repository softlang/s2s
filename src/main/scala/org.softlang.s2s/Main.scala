package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}

import org.rogach.scallop._

import scala.util.{Try, Failure}

/** Command line interface definition. */
class Conf(arguments: Seq[String]) extends ScallopConf(arguments):
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
      descrYes = "Remove output shapes entailed by others (Default: On)"
    )

  val log =
    toggle(
      default = Some(true),
      descrYes = "Print input and output as a log (Default: On)"
    )

  val hidecolon =
    toggle(
      default = Some(true),
      descrYes = "Hide colon in prefixes in log (Default: On)"
    )

  val debug =
    toggle(
      default = Some(false),
      descrYes = "Print inferred axioms as part of the log (Default: Off)"
    )

  val prettyVars =
    toggle(
      default = Some(true),
      descrYes = "Pretty variable concepts in log (Default: On)"
    )

  val output =
    toggle(
      default = Some(false),
      descrYes = "Print shapes as output (Default: Off)"
    )

  val prefix =
    opt[String](
      required = false,
      default = Some(":"),
      descr = "Standard prefix to use (Default ':')"
    )

  val queryFile = trailArg[String](descr = "File containing input query")

  val shapesFile = trailArg[String](
    required = false,
    descr = "File containing the input set of shapes",
    default = Some("")
  )

  verify()

  /** Convert to a S2S configuration. */
  def toConfiguration: Configuration = Configuration.default.copy(
    optimizeCandidates = optimize(),
    prefix = prefix(),
    log = log(),
    debug = debug(),
    hidecolon = hidecolon(),
    prettyVariableConcepts = prettyVars(),
    printOutput = output()
  )

@main def s2s(args: String*): Unit =

  // Initialize CLI configuration.
  val conf = Conf(args)

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

/*
// Compare two configurations of the algorithm.
def compare(): Unit =

  // Common settings for both Configurations.
  val common = Configuration.join(
    // Enable debugging.
    Configuration.debug,
    // Enable more formal output.
    Configuration.formalOutput,
    // Standard settings.
    Configuration(
      erasePvariables = false,
      eraseHvariables = false,
      approximatePvariables = false,
      approximateHvariables = false,
      useSubsumptionInPatternDCA = false,
      useSubsumptionInTemplateDCA = true,
      closeConcepts = true,
      closeProperties = true,
      closeTop = false,
      dcaForPattern = true,
      dcaForTemplate = true,
      // cwaForPattern = false,
      cwaForTemplate = true,
      unaForPattern = false,
      unaForTemplate = true,
      optimizeCandidates = true
    )
  )

  val compare = ConfigurationComparison(
    // Configuration 1...
    Configuration.join(
      // Common settings.
      common,
      // Custom settings for Configuration 1.
      Configuration(
        cwaForPattern = false
      )
    ),
    // ...compared vs. configuration 2.
    Configuration.join(
      // Common settings.
      common,
      // Custom settings for Configuration 2.
      Configuration(
        cwaForPattern = true
      )
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

  // (Option 1) Run structured comparison test.
  compare.structured()

  // (Option 2) Run comparison for a single case.

  // val example1 = (
  //  """
  //    CONSTRUCT {
  //      ?x a :B . ?y a :C
  //    } WHERE {
  //      ?x :p ?x . ?y a :A
  //    }
  //    """,
  //  Set(
  //    ":A ⊑ ∃:p.:A"
  //  )
  // )

  // compare.standalone(example1._1, example1._2)
 */
