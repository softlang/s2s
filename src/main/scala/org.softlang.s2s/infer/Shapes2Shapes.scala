package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.dl.Axiom

import org.softlang.s2s.core._
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.parser.ShapeParser
import org.softlang.s2s.parser.GCOREParser
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.GCORE

// TODO: Add more functionality to this API;
// simplify Algorithm.

/** Customizable implementation of the S2S algorithm. */
class Shapes2Shapes(config: Configuration = Configuration.default):

  // Instantiate the reasoning framework.
  val shar = Shar()
  import shar._

  // Set the s2s prefix.
  for
    p <- Prefix.fromString(config.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  // The scope encoding (implicit) needed for renaming.
  implicit val scopes: Scopes = Scopes(
    config.renameToken,
    // TODO: Change these IDs.
    in = 0,
    med = 1,
    out = 2,
    variable = -1)

  /** Remove any scope-related renaming. */
  protected def descope(shapes: Set[SHACLShape]): Set[SHACLShape] =
    shapes.map(_.dropScope)

  /** The SCCQ query parser. */
  private val sccqParser = SCCQParser(shar)

  /** Attempt to parse a SCCQ query. */
  def parseSCCQQuery(query: String): S2STry[SCCQ] =
    for
      qi <- sccqParser.parse(query)
      q <- SCCQ.validate(qi, config.renameToken)
    yield q

  /** The GCORE query parser. */
  private val gcoreParser = GCOREParser()

  // TODO: Add function that tries to parse one, 
  // then the other, and only fails if both parsers fail.

  /** Attempt to parse a GCORE query. */
  def parseGCOREQuery(query: String): S2STry[GCORE] =
    for 
      q <- gcoreParser(query)
    yield q

  /** The shape parser. */
  private val shapeParser = ShapeParser(shar)

  /** Attempt to parse a set of Simple SHACL shapes. */
  def parseSHACLShapes(
      shapes: Set[String]
  ): S2STry[Set[SHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapeParser.parse(_)).toList)
        .map(_.toSet)
    yield s.toList.toSet

  /** Run validation and format logging results and output (if enabled). */
  def run(
      query: String,
      shapes: Set[String]
  ): S2STry[Set[SHACLShape]] =

    // Run validation with query and shapes.
    val res = validate(query, shapes)
    val resLog = res._2
    val resShapes = res._1

    // Print log (if enabled).
    if config.log || config.debug then
      resLog.print(config.hidecolon, config.prettyVariableConcepts)

    // Remove internal scoping from output shapes.
    val output = resShapes.map(descope)

    // Print results (if enabled).
    if config.printOutput then
      for o <- output
      do o.map(_.show).toList.sorted.foreach(println)

    output

  /** Run validation and return the Log. */
  def validate(
      query: String,
      shapes: Set[String]
  ): (S2STry[Set[SHACLShape]], Log) =

    // Initialize the log.
    val log: Log = Log(debugging = config.debug)

    val sOut = for
      // Parse and validate query.
      q <- parseSCCQQuery(query)
      // Parse and validate input shapes.
      s <- parseSHACLShapes(shapes)
      // Run the algorithm.
      r <- algorithm(q, s, log)
    yield r

    // Output (first) error, if any.
    sOut.left.map(r => log.error(r.show))

    (sOut, log)

  /** Apply the algorithm, only. */
  def algorithmFast(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, 
      // Call with SCCQ and SimpleSHACLShapes.
      AlgorithmInput.SCCQSimpleSHACL(q, s.map(_.inScopeS(Scope.In))),
      log)(scopes)()

  /** Apply the algorithm, only. */
  def algorithm(
      q: SCCQ,
      s: Set[SHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, 
      // Call with SCCQ and Axioms.
      AlgorithmInput.SCCQAxioms(q, s.map(_.inScope(Scope.In).axiom.asInstanceOf[Axiom])),
      log)(scopes)()

  /** Apply the algorithm, only. */
  def algorithm(
      q: GCORE,
      s: Set[SHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, 
      // Call with SCCQ and Axioms.
      AlgorithmInput.GCOREAxioms(q, s.map(_.inScope(Scope.In).axiom.asInstanceOf[Axiom])),
      log)(scopes)()
