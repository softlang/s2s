package org.softlang.s2s

import org.softlang.s2s.core._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.{SCCQ, vocabulary}
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.parser.ShapeParser

import de.pseifer.shar.Shar
import de.pseifer.shar.reasoning.{AxiomSet, HermitReasoner}
import org.stringtemplate.v4.compiler.GroupParser.formalArgs_scope
import de.pseifer.shar.core.{Prefix, Iri}

/** Customizable implementation of the S2S algorithm. */
class Shapes2Shapes(config: Configuration = Configuration()):

  val shar = Shar()
  import shar._

  for
    p <- Prefix.fromString(config.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  private val sccqp = SCCQParser(shar)

  /** Attempt to parse a SCCQ query. */
  def parseQuery(query: String): ShassTry[SCCQ] =
    for
      qi <- sccqp.parse(query)
      q <- SCCQ.validate(qi)
    yield q

  private val shapep = ShapeParser(shar)

  /** Attempt to parse a set of Simple SHACL shapes. */
  def parseShapes(shapes: Set[String]): ShassTry[Set[SimpleSHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapep.parse(_)).toList)
        .map(_.toSet)
    yield s

  /** Run validation and format results (if enabled). */
  def run(
      query: String,
      shapes: Set[String]
  ): ShassTry[Set[SimpleSHACLShape]] =
    // Run validation with query and shapes.
    val res = validate(query, shapes)
    // Print log (if enabled).
    if config.log || config.debug then
      res._2.print(config.hidecolon, config.prettyVariableConcepts)
    // Return output shapes.
    res._1

  /** Run validation with a Log. */
  def validate(
      query: String,
      shapes: Set[String]
  ): (ShassTry[Set[SimpleSHACLShape]], Log) =

    // Initialize the log.
    val log: Log = Log(debugging = config.debug)

    val sout = for
      // Parse and validate query.
      q <- parseQuery(query)
      // Parse and validate input shapes.
      s <- parseShapes(shapes)
    // Run the algorithm.
    yield algorithm(q, s, log)

    // Output (first) error, if any.
    sout.left.map(r => log.error(r.show))

    (sout, log)

  /** Run algorithm with formal input (given a log). */
  def algorithm(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): Set[SimpleSHACLShape] =

    // Log input query and shapes.
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

    // Initialize the reasoner.
    val hermit = buildKB(q, s, log)

    // Generate candidate shapes.
    val cand = CandidateGenerator(
      q.template.vocabulary,
      optimize = config.optimizeCandidates
    ).axioms

    log.debug("S_can", cand.map(_.show).toList)

    // Yield the validated subset of candidates.
    val out = cand.filter(si => hermit.prove(si.axiom))
    log.info("S_out", out.map(_.show).toList)

    out

  /** Perform the KB construction set of the algorithm. */
  def buildKB(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): HermitReasoner =

    // DCA for query pattern.

    val dcaP =
      if config.dcaForPattern then
        DomainClosureAssumption(
          q.pattern,
          eraseVariables = config.erasePvariables,
          approximateVariables = config.approximatePvariables
        ).axioms
      else Set()

    if config.dcaForPattern then log.debug("DCA(q.P)", dcaP.map(_.show).toList)

    // DCA for query template.

    val dcaH =
      if config.dcaForTemplate then
        DomainClosureAssumption(
          q.template,
          eraseVariables = config.eraseHvariables,
          approximateVariables = config.approximateHvariables
        ).axioms
      else Set()

    if config.dcaForTemplate then log.debug("DCA(q.H)", dcaH.map(_.show).toList)

    // CWA for query pattern.

    val cwaP =
      if config.cwaForPattern then
        ClosedWorldAssumption(
          q.pattern,
          config.closeConcepts,
          config.closeProperties,
          config.closeTop
        ).axioms
      else Set()

    if config.cwaForPattern then log.debug("CWA(q.P)", cwaP.map(_.show).toList)

    // CWA for query template.

    val cwaH =
      if config.cwaForTemplate then
        ClosedWorldAssumption(
          q.template,
          config.closeConcepts,
          config.closeProperties,
          config.closeTop
        ).axioms
      else Set()

    if config.cwaForTemplate then log.debug("CWA(q.H)", cwaH.map(_.show).toList)

    // UNA for query pattern.

    val unaP =
      if config.unaForPattern then UniqueNameAssumption(q.pattern).axioms
      else Set()

    if config.unaForPattern then log.debug("UNA(q.P)", unaP.map(_.show).toList)

    // UNA for query template.

    val unaH =
      if config.unaForTemplate then UniqueNameAssumption(q.template).axioms
      else Set()

    if config.unaForTemplate then log.debug("UNA(q.H)", unaH.map(_.show).toList)

    // Initialize the reasoner.
    val hermit = HermitReasoner.default
    hermit.addAxioms(
      AxiomSet(
        s.map(_.axiom)
          .union(dcaP)
          .union(dcaH)
          .union(cwaP)
          .union(unaP)
          .union(cwaH)
          .union(unaH)
      )
    )
    hermit
