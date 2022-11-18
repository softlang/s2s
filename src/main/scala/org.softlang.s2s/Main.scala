package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}

import org.rogach.scallop._

import scala.util.{Try, Failure}

/** Command line interface definition. */
class Conf(arguments: Seq[String]) extends ScallopConf(arguments):
  version("Shapes2Shapes 0.0.1 - Philipp Seifer @ Softlang, University of Koblenz")
  banner("""Usage: s2s [OPTION]... [query-file] [shapes-file?]
           |Options:
           |""".stripMargin)
  footer("\n" + 
    """For more information about the algorithm, read the full paper
      |    Philipp Seifer, Daniel Hernandez, Ralf Lämmel and Steffen Staab:
      |    FromFrom Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines
      |    ...TBD...
      |""".stripMargin)

  val optimizeCandidates = 
    toggle(default = Some(true),
           descrYes = "Remove some output shapes entailed by others")

  val log = 
    toggle(default = Some(true), 
           descrYes = "Print input and output")

  val hidecolon = 
    toggle(default = Some(true), 
           descrYes = "Hide (prefix) colon in log")

  val debug = 
    toggle(default = Some(false), 
           descrYes = "Print internal state")

  val prettyVariableConcepts = 
    toggle(default = Some(true), 
           descrYes = "Prettier debug log")

  val output = 
    toggle(default = Some(false), 
           descrYes = "Print output shapes")

  val prefix = 
    opt[String](required = false, default = Some(":"),
                descr = "The standard prefix")

  val queryFile = trailArg[String](descr = "The input query")

  val shapesFile = trailArg[String](required = false, 
    descr = "The set of input shapes", 
    default = Some(""))

  verify()

  /** Convert to a S2S configuration. */
  def toConfiguration: Configuration = Configuration.join(
    Configuration(
      optimizeCandidates = optimizeCandidates(),
      log = log(),
      hidecolon = hidecolon(),
      prettyVariableConcepts =  prettyVariableConcepts(),
      debug = debug(),
      prefix = prefix()
    ),
    Configuration.default)


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
  do Shapes2Shapes(conf.toConfiguration).run(q, s, print = conf.output())



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
