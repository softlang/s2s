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

class Shapes2Shapes(config: Configuration = Configuration()):

  val shar = Shar()
  import shar._

  for
    p <- Prefix.fromString(config.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  val sccqp = SCCQParser(shar)

  def parseQuery(query: String): ShassTry[SCCQ] =
    for
      qi <- sccqp.parse(query)
      q <- SCCQ.validate(qi)
    yield q

  val shapep = ShapeParser(shar)

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

  /** Run algorithm with a Log. */
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

  def buildKB(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): HermitReasoner =
    // Generate axioms.
    val dcaP = DomainClosureAssumption(
      q.pattern,
      eraseVariables = config.erasePvariables,
      approximateVariables = config.approximatePvariables
    ).axioms

    log.debug("DCA(q.P)", dcaP.map(_.show).toList)

    val dcaH = DomainClosureAssumption(
      q.template,
      eraseVariables = config.eraseHvariables,
      approximateVariables = config.approximateHvariables
    ).axioms

    log.debug("DCA(q.H)", dcaH.map(_.show).toList)

    val cwa = ClosedWorldAssumption(q.template).axioms

    log.debug("CWA(q)", cwa.map(_.show).toList)

    val una = UniqueNameAssumption(q.template).axioms

    log.debug("UNA(q)", una.map(_.show).toList)

    // Initialize the reasoner.
    val hermit = HermitReasoner.default
    hermit.addAxioms(
      AxiomSet(
        s.map(_.axiom)
          .union(dcaP)
          .union(dcaH)
          .union(cwa)
          .union(una)
      )
    )
    hermit
