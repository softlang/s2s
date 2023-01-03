package org.softlang.s2s

import org.rogach.scallop._
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.Log
import org.softlang.s2s.parser.JsonLDToSimpleShacl

import scala.util.Failure
import scala.util.Try

/** Command line interface definition. */
class Conf(baseConfiguration: Configuration, arguments: Seq[String])
    extends ScallopConf(arguments):
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
      default = Some("٭"),
      descr = "Use this String for internal renaming (def: *)"
    )

  val topSymbol =
    opt[String](
      required = false,
      default = Some("T"),
      descr = "Use this String for internal Top (def: T)"
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
    renameToken = renameToken(),
    namespacedTopName = topSymbol(),
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
  val conf = Conf(Configuration.default, args)

  // Create buffered sources and check for errors.
  val qft = Try(io.Source.fromFile(conf.queryFile()).getLines.mkString("\n"))
  val sft = Try(
    if conf.shapesFile().isEmpty then Set()
    else if conf.shapesFile().contains(".json") then
      JsonLDToSimpleShacl(conf.shapesFile()).convert
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
      useNamespacedTop = true
    ),
    // Configuration 2:
    common.copy(
      useNamespacedTop = false
    ),
    // Perform 1000 trials per generator configuration.
    trials = 1,
    // Generate multiple results.
    stopAfterFirstResult = false
  )

  compare.structured()

@main def dev(): Unit =
  import org.softlang.s2s.generate._
  import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
  import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
  import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
  import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange

  val s2s = Shapes2Shapes(Configuration.default)
  val config =
    ProblemGeneratorConfig(
      // Min/max count of atomic patterns in Pattern.
      minPatternSize = (3, 5),
      maxPatternSize = (5, 7),
      // Min/max count of atomic patterns in Template.
      minTemplateSize = 3,
      maxTemplateSize = 5,
      // Probability of generating a fresh variable (0.0 to 1.0).
      freshVariable = 0.8f,
      // Maximum number of variables.
      variablesCount = 2,
      // Probability of generating a fresh concept (0.0 to 1.0).
      freshConcept = 1.0f,
      // Total number of concepts allowed.
      conceptsCount = 10,
      // Probability of generating a fresh property (0.0 to 1.0).
      freshProperty = 1.0f,
      // Total number of properties allowed.
      propertiesCount = 10,
      // Probability of generating a fresh nominal (0.0 to 1.0).
      freshNominal = 0.0f,
      // Total number of nominals allowed.
      nominalsCount = 0,
      // Ratio of property patterns to concept patterns (0.0 to 1.0).
      propertyConceptRatio = 0.5f,
      // Ratio of variables to nominals in patterns (0.0 to 1.0).
      variableToNominalRatio = 1.0f,
      // Avoid self-circles by redrawing N times.
      cyclicRedrawCount = 10,
      // Min/max number of input shapes.
      minNumberOfShapes = 8,
      maxNumberOfShapes = 10,
      // Ratio of property-based vs. Concept targets.
      propertyConceptTargetRatio = -1.0f,
      // Ratio of property (exists, forall) vs. Concept constraints.
      propertyConceptConstraintRatio = -1.0f,
      // Ratio of existential vs. universal quantification in constraints.
      includeForallConstraints = false
    )

  println(config)

  val gen = ProblemGenerator(config)(s2s.scopes)

  val qs = gen.sample()
  val q = qs._1
  val s = qs._2

  println(q.show(s2s.shar.state))
  s.foreach(si => println(si.show(s2s.shar.state)))
