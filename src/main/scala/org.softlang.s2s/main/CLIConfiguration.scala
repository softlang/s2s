package org.softlang.s2s.main

import org.rogach.scallop._
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.ActiveReasoner

/** Command line interface definition. */
class CLIConfiguration(baseConfiguration: Configuration, arguments: Seq[String])
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
      default = Some(false),
      descrYes = "Remove output shapes entailed by others (def: Off)"
    )

  val log =
    toggle(
      default = Some(false),
      descrYes = "Print input and output as a log (def: Off)"
    )

  val hidecolon =
    toggle(
      default = Some(true),
      descrYes = "Hide colon in prefixes in log (def: On)"
    )

  val timeout =
    opt[Long](
      required = false,
      default = Some(0),
      descr = "Timeout for reasoning attempt (def: 0 -- disabled)"
    )

  val retry =
    opt[Int](
      required = false,
      default = Some(0),
      descr = "Retries after timeout (def: 0)"
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
      default = Some(true),
      descrYes = "Print shapes as output (def: On)"
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

  val reasoner = choice(
    choices = Seq("hermit", "jfact", "openllet"),
    descr = "Reasoner to use (def: hermit).",
    default = Some("hermit")
  )

  verify()

  /** Convert to a S2S configuration. */
  def toConfiguration: Configuration = baseConfiguration.copy(
    optimizeCandidates = optimize(),
    renameToken = renameToken(),
    activeReasoner = ActiveReasoner.fromString(reasoner()),
    retry = retry(),
    timeout = timeout(),
    namespacedTopName = topSymbol(),
    prefix = prefix(),
    log = log(),
    debug = debug(),
    hidecolon = hidecolon(),
    prettyVariableConcepts = prettyVars(),
    printOutput = output()
  )
