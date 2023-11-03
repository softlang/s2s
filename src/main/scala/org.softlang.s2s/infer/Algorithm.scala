package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.dl.Equality
import de.pseifer.shar.dl.Subsumption
import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.reasoning._
import org.softlang.s2s.core._
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary

import scala.concurrent.duration.*

// TODO (1) Input should not be a set of shapes,
//          but rather the prior KB.
// TODO (2) Should compose w.r.t. namespaces.

/** Full implementation of Algorithm 1. */
class Algorithm(
    val config: Configuration,
    val shar: Shar,
    query: SCCQ,
    shapes: Set[SHACLShape],
    log: Log
)(implicit val scopes: Scopes):

  import shar._

  /** Run algorithm with formal input (given a log). */
  def apply(): S2STry[Set[SHACLShape]] =

    log.profileStart("algorithm")

    // Pre-process query and shapes, by setting scopes.
    val preQ = SCCQ(
      query.template.inScope(Scope.Out),
      query.pattern.inScope(Scope.Med)
    )
    val pres = shapes.map(_.inScope(Scope.In))

    // Log input.
    logInput(preQ, pres, log)
    log.profileStart("build")

    // Infer axioms from query and shapes.
    val initialAxioms = buildAxioms(preQ, pres, log)
    val extendedAxioms = extendAxioms(preQ, pres, initialAxioms, log)
    val axioms = AxiomSet(initialAxioms.union(extendedAxioms))

    log.profileEnd("build")
    log.profileStart("candidates")
    log.profileStart("filter")

    val canGen = CandidateGenerator(
      preQ.template.vocabulary,
      heuristic = config.shapeHeuristic
    )(scopes)
    var result: S2STry[Set[SHACLShape]] = Right(Set())
    var all: Set[SHACLShape] = Set()
    var current = canGen.getNext()

    while current.nonEmpty do
      val previous =
        filter(current, axioms, log, config.retry, config.timeout.millis)
      result = for
        p <- previous
        r <- result
      yield r.union(p)
      all = all.union(current)
      current = canGen.getNext(previous)

    log.candidates(current)

    log.profileEnd("candidates")
    log.profileEnd("filter")
    log.profileEnd("algorithm")

    result

  /** Perform the KB construction set of the algorithm. */
  def buildAxioms(
      q: SCCQ,
      s: Set[SHACLShape],
      log: Log
  ): Set[Axiom] =

    log.profileStart("build-dca-p")

    // DCA for query pattern.

    val dcaP = ClosedConceptAssumptionPattern(q.pattern).axioms

    // Step 1 (in the Paper), relevant for (debug) info only.
    val dcaP1 = dcaP.filter(a =>
      a match
        case Equality(_, _) => true
        case _              => false
    )

    // Step 3 (in the Paper), relevant for (debug) info only.
    val dcaP3 = dcaP.filter(a =>
      a match
        case Subsumption(_, _) => true
        case _                 => false
    )

    log.debug("CWA(q.P), step 1", dcaP1)

    log.profileEnd("build-dca-p")
    log.profileStart("build-dca-t")

    // DCA for query template.

    val dcaH = ClosedConceptAssumptionTemplate(q.template).axioms

    log.debug("CWA(q.H), step 2.", dcaH)
    log.debug("CWA(q.P), step 3.", dcaP3)

    log.profileEnd("build-dca-t")
    log.profileStart("build-cwa-p")

    // CWA for query pattern.

    val cwaP = ClosedPropertyAssumption(q.pattern, Scope.Med).axioms
    log.debug("CWA(q.P), step 4.", cwaP)

    log.profileEnd("build-cwa-p")
    log.profileStart("build-cwa-t")

    // CWA for query template.

    val cwaH = ClosedPropertyAssumption(q.template, Scope.Out).axioms
    log.debug("CWA(q.H), step 5.", cwaH)

    log.profileEnd("build-cwa-t")
    log.profileStart("build-una-p")

    // UNA for query template & pattern.

    val una = UniqueNameAssumption(q.template.union(q.pattern)).axioms
    log.debug("UNA(q)", una)

    log.profileEnd("build-una")

    // Property subsumption.

    val shapeProps = ShapePropertySubsumption(q.pattern).axioms
    log.debug("RS(q, S_in)", shapeProps)

    log.profileEnd("build-properties")

    // Return the complete set of axioms.

    s.map(_.axiom)
      .union(shapeProps)
      .union(dcaP)
      .union(dcaH)
      .union(cwaP)
      .union(cwaH)
      .union(una)

  /** Perform the KB construction extension step of the algorithm. */
  def extendAxioms(
      q: SCCQ,
      s: Set[SHACLShape],
      ax: Set[Axiom],
      log: Log
    ): Set[Axiom] =

    log.profileStart("build-mapping")

    // Axioms from mapping method.
    
    // Make shapes from candidates over (input scope) vocabulary of query.
    val canGen = CandidateGenerator(
      q.pattern.vocabulary,
      heuristic = ShapeHeuristic(
        optimize = false,
        proxyFamily = true,
        simpleShapes = true)
    )(scopes)
    var result: S2STry[Set[SHACLShape]] = Right(Set())
    var all: Set[SHACLShape] = Set()
    var current = canGen.getNext().map(_.inScope(Scope.In))

    while current.nonEmpty do
      val previous =
        filter(current, AxiomSet(ax), Log(), config.retry, config.timeout.millis)
      result = for
        p <- previous
        r <- result
      yield r.union(p)
      all = all.union(current)
      current = canGen.getNext(previous)

    //println("\n\n")
    //println("-------------------------------------------")
    //result.toOption.get.map(_.show).foreach(println)
    //println("===")
    //s.map(_.show).foreach(println)

    log.debug("ShApEs ", all.map(_.show).mkString(","))
    log.debug("ShApEs ", result.toOption.get.map(_.show).mkString(","))

    val mappingSubs = SubsumptionsFromMappings(
      q.pattern,
      result.toOption.get.map(_.toSimple).filter(_.nonEmpty).map(_.get)
      //s.map(_.toSimple).filter(_.nonEmpty).map(_.get)
    ).axioms
    log.debug("MA(S_in, q.P)", mappingSubs)

    log.profileEnd("build-mapping")
    log.profileStart("build-properties")

    // Add property subsumptions within query, and between query and shapes.

    val props = PropertySubsumption(q.pattern, mappingSubs, q.template).axioms
    log.debug("RS(q)", props)

    // Return the extended set of axioms.

    mappingSubs.union(props)

  /** Add input query and shapes to log. */
  private def logInput(q: SCCQ, s: Set[SHACLShape], log: Log): Unit =
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

  /** Perform the filtering step of the algorithm. */
  private def filter(
      candidates: Set[SHACLShape],
      axioms: AxiomSet,
      log: Log,
      retry: Int,
      timeout: Duration,
      currentTry: Int = 1
  ): S2STry[Set[SHACLShape]] =

    // A fresh log.
    val pLog = Log(debugging = config.debug)

    // A fresh reasoner.
    val reasoner = config.reasoner.create

    reasoner.addAxioms(axioms)
    val result = filterWithTimeout(candidates, reasoner, pLog, timeout)

    // Wait configured timeout for completion.
    result match
      case Some(pr) =>
        // Include log only of the completed run / if completed.
        log.append(pr._1)
        // Return the filtered set of shapes.
        Right(pr._2)
      case None =>
        if currentTry <= retry then
          log.restart("filter", currentTry, retry, timeout)
          filter(candidates, axioms, log, retry, timeout, currentTry + 1)
        else
          log.timeout("filter", retry, timeout)
          Left(TimeoutError(config.timeout, config.retry))

  /** Filter, with provided timeout value that aborts filtering. */
  private def filterWithTimeout(
      candidates: Set[SHACLShape],
      reasoner: DLReasoner,
      log: Log,
      timeout: Duration
  ): Option[(Log, Set[SHACLShape])] =

    var result: Option[Set[SHACLShape]] = None

    import java.time.LocalDateTime

    val t = new Thread {
      override def run(): Unit =
        result = Some(candidates.filter(si => reasoner.prove(si.axiom)))
    }

    t.start()
    t.join(timeout.toMillis)
    t.stop()

    if result.isDefined then log.info("S_out", result.get.map(_.show))
    result.map((log, _))
