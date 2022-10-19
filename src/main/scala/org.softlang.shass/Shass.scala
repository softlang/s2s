package org.softlang.shass

import org.softlang.shass.core.{Util, Log, ShassTry, SimpleSHACLShape}
import org.softlang.shass.infer._
import org.softlang.shass.query.{SCCQ, vocabulary}
import org.softlang.shass.generate.CandidateGenerator
import org.softlang.shass.parser.SCCQParser
import org.softlang.shass.parser.ShapeParser

import de.pseifer.shar.Shar
import de.pseifer.shar.reasoning.{AxiomSet, HermitReasoner}
import org.stringtemplate.v4.compiler.GroupParser.formalArgs_scope
import de.pseifer.shar.core.{Prefix, Iri}

class Shass(
    log: Boolean = true,
    debug: Boolean = false,
    prefix: String = "shass:",
    hidecolon: Boolean = false
):

  val shar = Shar()
  import shar._

  for
    p <- Prefix.fromString(prefix)
    i <- Iri.fromString("<https://github.com/softlang/shass/>")
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
    if log || debug then res._2.print(hidecolon)
    // Return output shapes.
    res._1

  /** Run validation with a Log. */
  def validate(
      query: String,
      shapes: Set[String],
      log: Log = Log(info = log, debugging = debug)
  ): (ShassTry[Set[SimpleSHACLShape]], Log) =
    val sout = for
      // Parse and validate query.
      q <- parseQuery(query)
      _ = log.info("q", q.show)
      _ = log.debug("Σ(q)", q.vocabulary.show)
      // Parse and validate input shapes.
      s <- parseShapes(shapes)
      _ = log.info("S_in", s.map(_.show).toList)
      // Generate axioms.
      dcaP = DomainClosureAssumption(q.pattern).axioms
      _ = log.debug("DCA(q.P)", dcaP.map(_.show).toList)
      dcaH = DomainClosureAssumption(q.template).axioms
      _ = log.debug("DCA(q.H)", dcaH.map(_.show).toList)
      cwa = ClosedWorldAssumption(q.template).axioms
      _ = log.debug("CWA(q)", cwa.map(_.show).toList)
      una = UniqueNameAssumption(q.template).axioms
      _ = log.debug("UNA(q)", una.map(_.show).toList)
      // Generate candidate shapes.
      cand = CandidateGenerator(q.template.vocabulary).axioms(optimize = true)
      _ = log.debug("S_can", cand.map(_.show).toList)
      // Initialize the reasoner.
      hermit = HermitReasoner.default
      _ = hermit.addAxioms(
        AxiomSet(
          s.map(_.axiom)
            .union(dcaP)
            .union(dcaH)
            .union(cwa)
            .union(una)
        )
      )
    // Yield the validated subset of candidates.
    yield cand.filter(si => hermit.prove(si.axiom))

    // Print (first) error, or...
    sout.left.map(r => log.error(r.show))

    // Print the results, if no error occurred.
    for shapes <- sout
    do log.info("S_out", shapes.map(_.show).toList)

    (sout, log)
