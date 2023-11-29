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

/** Customizable implementation of the S2S algorithm. */
class Shapes2Shapes(private var config: Configuration = Configuration.default):

  // Instantiate the reasoning framework.
  val shar = Shar()
  import shar._

  // Set the s2s prefix.
  for
    p <- Prefix.fromString(config.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  // The default scope encoding needed for renaming.
  // If input contains Axioms, the given scopes are used instead.
  val defaultScopes: Scopes = Scopes.default(config.renameToken)

  /** Remove any scope-related renaming. */
  protected def descope(shapes: Set[SHACLShape]): Set[SHACLShape] =
    shapes.map(_.dropScope(defaultScopes))

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

  /** Attempt to parse a GCORE query. */
  def parseGCOREQuery(query: String): S2STry[GCORE] =

    // Add GCORE prefixes.
    for
      pl <- Prefix.fromString("l:")
      il <- Iri.fromString("<https://github.com/softlang/s2s/label/>")
      pk <- Prefix.fromString("k:")
      ik <- Iri.fromString("<https://github.com/softlang/s2s/key/>")
    do {
      shar.state.prefixes.add(pl, il)
      shar.state.prefixes.add(pk, ik)
    }
    
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
  ): Unit =

    // Run validation with query and shapes.
    val res = constructShapes(query, shapes)
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

  /** Run validation and return shapes and the Log. */
  def constructShapes(
    query: String,
    shapes: Set[String]
  ): (S2STry[Set[SHACLShape]], Log) =
    constructWith(query, shapes, algorithmInternalShapes)

  /** Run axiom construction and return them and the Log. */
  def constructShapes(
    input: AlgorithmInput
  ): (S2STry[Set[SHACLShape]], Log) =
    val log = Log(debugging = config.debug)(defaultScopes)
    (algorithmInternalShapes(input, log), log)

  /** Run axiom construction and return them and the Log. */
  def constructAxioms(
    query: String,
    shapes: Set[String]
  ): (S2STry[Axioms], Log) =
    constructWith(query, shapes, algorithmInternalAxioms)

  /** Run axiom construction and return them and the Log. */
  def constructAxioms(
    input: AlgorithmInput
  ): (S2STry[Axioms], Log) =
    val log = Log(debugging = config.debug)(defaultScopes)
    (algorithmInternalAxioms(input, log), log)

  // Internal Algorithm wrapper.

  /** Construct shapes or axioms, using 'fn'. */
  private def constructWith[T](
      query: String,
      shapes: Set[String],
      fn: (AlgorithmInput, Log) => S2STry[T]
  ): (S2STry[T], Log) =

    // Initialize the log.
    val log: Log = Log(debugging = config.debug)(defaultScopes)

    val sOut = for
      // Parse and validate query.
      q <- parseSCCQQuery(query).map(q => sa => AlgorithmInput.SCCQAxioms(q, sa)).orElse {
        parseGCOREQuery(query).map(q => sa => AlgorithmInput.GCOREAxioms(q, sa))
      }
      // Parse and validate input shapes. Note: Depends on order! GCORE prefixes
      // are only set on GCORE parse (i.e., if SCCQ parse fails).
      s <- parseSHACLShapes(shapes)
      sa = Axioms(s.map(_.inScope(Scope.In)(defaultScopes).axiom.asInstanceOf[Axiom]), defaultScopes)
      // TODO: Use simple shape input, if enabled / valid.
      // Run the algorithm.
      r <- fn(q(sa), log)
    yield r

    // Output (first) error, if any.
    sOut.left.map(r => log.error(r.show))

    (sOut, log)

  /** Apply the algorithm with given AlgorithmInput. */
  private def algorithmInternalShapes(
      input: AlgorithmInput,
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, input, log).shapes

  /** Apply the algorithm with given AlgorithmInput. */
  private def algorithmInternalAxioms(
      input: AlgorithmInput,
      log: Log
  ): S2STry[Axioms] = 
    Algorithm(config, shar, input, log).axioms

  // Functions for internal development tools, profiling, and testing.

  /** Apply the algorithm, only, with SimpleSHACLShapes. */
  protected def algorithmFast(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, 
      // Call with SCCQ and SimpleSHACLShapes.
      AlgorithmInput.SCCQSimpleSHACL(
        q, 
        s.map(_.inScopeS(Scope.In)(defaultScopes)),
        defaultScopes),
      log).shapes

  /** Apply the algorithm, only. */
  protected def algorithm(
      q: SCCQ,
      s: Set[SHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    Algorithm(config, shar, 
      // Call with SCCQ and Axioms.
      AlgorithmInput.SCCQAxioms(
        q, 
        Axioms(s.map(_.inScope(Scope.In)(defaultScopes).axiom.asInstanceOf[Axiom]), defaultScopes)
      ), log).shapes

  /** Apply the algorithm, only. */
  protected def algorithm(
      q: GCORE,
      s: Set[SHACLShape],
      log: Log
  ): S2STry[Set[SHACLShape]] = 
    // Call algorithm with GCORE query and axioms.
    Algorithm(config, shar, 
      AlgorithmInput.GCOREAxioms(
        q,
        Axioms(s.map(_.inScope(Scope.In)(defaultScopes).axiom.asInstanceOf[Axiom]), defaultScopes))
      , log).shapes

  /** Set the config. */
  protected def setConfig(config: Configuration): Unit =
    this.config = config

