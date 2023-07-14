package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.dl.Equality
import de.pseifer.shar.dl.Subsumption
import de.pseifer.shar.reasoning._
import org.softlang.s2s.core._
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.parser.ShapeParser
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary

import scala.concurrent.duration.*

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
  implicit val scopes: Scopes = Scopes(config.renameToken)

  /** Remove any scope-related renaming. */
  protected def descope(shapes: Set[SHACLShape]): Set[SHACLShape] =
    shapes.map(_.dropScope)

  /** The query parser. */
  private val sccqp = SCCQParser(shar)

  /** Attempt to parse a SCCQ query. */
  def parseQuery(query: String): S2STry[SCCQ] =
    for
      qi <- sccqp.parse(query)
      q <- SCCQ.validate(qi, config.renameToken)
    yield q

  /** The shape parser. */
  private val shapep = ShapeParser(shar)

  /** Attempt to parse a set of Simple SHACL shapes. */
  def parseShapes(
      shapes: Set[String]
  ): S2STry[Set[SHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapep.parse(_)).toList)
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

    val sout = for
      // Parse and validate query.
      q <- parseQuery(query)
      // Parse and validate input shapes.
      s <- parseShapes(shapes)
      // Run the algorithm.
      r <- algorithm(q, s, log)
    yield r

    // Output (first) error, if any.
    sout.left.map(r => log.error(r.show))

    (sout, log)

  /** Apply the algorithm, only. */
  def algorithm(
      q: SCCQ,
      s: Set[SHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = Algorithm(config, shar, q, s, log)(scopes)()
