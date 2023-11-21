package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.dl.Equality
import de.pseifer.shar.dl.Subsumption
import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.reasoning._
import org.softlang.s2s.core._
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.query.AtomicPatterns
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary

import scala.concurrent.duration.*

/** Specifies the possible inputs for the Algorithm. */
enum AlgorithmInput:
  /** A SCCQ query and a set of Axioms. */
  case SCCQAxioms(q: SCCQ, axioms: Set[Axiom] = Set())

  /** A SCCQ query and a set of SimpleSHACLShapes. */
  case SCCQSimpleSHACL(q: SCCQ, axioms: Set[SimpleSHACLShape] = Set())

  /** A GCORE query and a set of Axioms. */
  case GCOREAxioms(q: GCORE, axioms: Set[Axiom] = Set())

  /** Get the template of the input query. */
  def template(implicit scopes: Scopes): S2STry[AtomicPatterns] = this match
    case SCCQAxioms(q, _) => Right(q.template.inScope(Scope.Out))
    case SCCQSimpleSHACL(q, _) => Right(q.template.inScope(Scope.Out))
    case GCOREAxioms(q, _) => q.sccqTemplate match
      case None => Left(
        UnsupportedQueryError(q, details = "This GCORE query can not be converted to SPARQL.")
      )
      case Some(t) => Right(t.inScope(Scope.Out))

  /** Get the pattern of the input query. */
  def pattern(implicit scopes: Scopes): S2STry[AtomicPatterns] = this match
    case SCCQAxioms(q, _) => Right(q.pattern.inScope(Scope.Med))
    case SCCQSimpleSHACL(q, _) =>  Right(q.pattern.inScope(Scope.Med))
    case GCOREAxioms(q, _) => q.sccqPattern match
      case None => Left(
        UnsupportedQueryError(q, details = "This GCORE query can not be converted to SPARQL.")
      )
      case Some(p) => Right(p.inScope(Scope.Med))

  /** Return input constraints as axioms. */
  def shapeAxioms: Set[Axiom] = this match
    case SCCQAxioms(_, ax) => ax
    case SCCQSimpleSHACL(_, s) => s.map(_.axiom)
    case GCOREAxioms(_, ax) => ax

  /** Get shapes for extension steps; use 'convert', if not explicit. */
  def extensionShapes(converted: () => S2STry[Set[SHACLShape]]): S2STry[Set[SHACLShape]] = 
    this match
      case SCCQSimpleSHACL(_, s) => Right(s.map(_.asInstanceOf[SHACLShape]))
      case SCCQAxioms(_, _) => converted()
      case GCOREAxioms(_, _) => converted()

/** Full implementation of Algorithm 1. */
class Algorithm(
    // The configuration to use.
    val config: Configuration,
    // The given shar backend state.
    val shar: Shar,
    // Input, as instance of AlgorithmInput.
    input: AlgorithmInput,
    // The log to use.
    log: Log
)(implicit val scopes: Scopes):

  import shar._

  /** Build axioms for the query pattern. */
  def buildAxiomsPattern(pattern: AtomicPatterns, log: Log): Set[Axiom] = 
    log.profileStart("build-dca-p")

    // DCA for query pattern.

    val dcaP = ClosedConceptAssumptionPattern(pattern).axioms

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
    log.debug("CWA(q.P), step 3.", dcaP3)
    log.profileEnd("build-dca-p")

    // CWA for query pattern.

    log.profileStart("build-cwa-p")

    val cwaP = ClosedPropertyAssumption(pattern, Scope.Med).axioms
    log.debug("CWA(q.P), step 4.", cwaP)

    log.profileEnd("build-cwa-p")

    // Property subsumption.
    
    log.profileStart("build-properties")

    val shapeProps = ShapePropertySubsumption(pattern).axioms
    log.debug("RS(q, S_in)", shapeProps)

    log.profileEnd("build-properties")

    // Return union of components.
    shapeProps
      .union(dcaP)
      .union(cwaP)

  /** Process the query pattern. */
  def processPattern(log: Log): S2STry[Set[Axiom]] = 
    for 
      p <- input.pattern
    yield buildAxiomsPattern(p, log)

  /** Build axioms for the query pattern. */
  def buildAxiomsTemplate(template: AtomicPatterns, pattern: AtomicPatterns, log: Log): Set[Axiom] = 

    // DCA for query template.
    log.profileStart("build-dca-t")

    val dcaH = ClosedConceptAssumptionTemplate(template).axioms
    log.debug("CWA(q.H), step 2.", dcaH)

    log.profileEnd("build-dca-t")

    // CWA for query template.

    log.profileStart("build-cwa-t")

    val cwaH = ClosedPropertyAssumption(template, Scope.Out).axioms
    log.debug("CWA(q.H), step 5.", cwaH)

    log.profileEnd("build-cwa-t")

    // UNA for query template & pattern.

    log.profileStart("build-una-p")

    val una = UniqueNameAssumption(template.concat(pattern)).axioms
    log.debug("UNA(q)", una)

    log.profileEnd("build-una")

    // Return the complete set of axioms.
    dcaH.union(cwaH).union(una)

  /** Process the query template. */
  def processTemplate(log: Log): S2STry[Set[Axiom]] = 
    for 
      t <- input.template
      p <- input.pattern
    yield buildAxiomsTemplate(
      template = t, 
      pattern = p, 
      log)

  /** Run algorithm with formal input (given a log). */
  def apply(): S2STry[Set[SHACLShape]] =
    //log.profileEnd("build")
    //log.profileStart("candidates")
    //log.profileStart("filter")

    for 
      axioms <- axioms()
      t <- input.template
      canGen = CandidateGenerator(
        t.vocabulary,
        heuristic = config.shapeHeuristic
      )(scopes)
      result <- {
        var result: S2STry[Set[SHACLShape]] = Right(Set())
        var all: Set[SHACLShape] = Set()
        var current = canGen.getNext()

        while current.nonEmpty do
          val previous =
            filter(current, AxiomSet(axioms), log, config.retry, config.timeout.millis)
          result = for
            p <- previous
            r <- result
          yield r.union(p)
          all = all.union(current)
          current = canGen.getNext(previous)
        result
      }
    yield result

    //log.candidates(current)

    //log.profileEnd("candidates")
    //log.profileEnd("filter")
    //log.profileEnd("algorithm")

  def axioms(): S2STry[Set[Axiom]] =

    //log.profileStart("algorithm")

    // Log input. TODO
    // if inConstraints.isLeft then 
    //   logInput(preQ, inConstraints.swap.toOption.get.map(_.asInstanceOf[SHACLShape]), log)
    // log.profileStart("build")

    for
      // Infer axioms from the query pattern.
      patternAxioms <- processPattern(log)
      // Transform inout shapes (or axioms) to axioms.
      shapeAxioms = input.shapeAxioms
      // Generate axioms from mapping components, using previous axioms.
      mappingSubs <- extendMapping(shapeAxioms.union(patternAxioms), log)
      // Generate axioms from template.
      templateAxioms <- processTemplate(log)
      // Generate axioms for properties.
      props <- extendProperties(mappingSubs, log)
      // Finally, join all axioms inferred here.
      axioms = patternAxioms
        .union(shapeAxioms)
        .union(mappingSubs)
        .union(templateAxioms)
        .union(props)
    yield axioms 

  /** Perform the KB construction extension step of the algorithm. */
  def extendMapping(initialAxioms: Set[Axiom], log: Log): S2STry[Set[Axiom]] =
    for 
      shapes <- input.extensionShapes(() => convert(initialAxioms))
      p <- input.pattern
      mappingSubs = {
        log.profileStart("build-mapping")
        val mappingSubs = SubsumptionsFromMappings(
          p, shapes.map(_.toSimple).filter(_.nonEmpty).map(_.get)
        ).axioms
        log.debug("MA(S_in, q.P)", mappingSubs)
        log.profileEnd("build-mapping")
        mappingSubs
      }
    yield mappingSubs

  /** Perform the KB construction extension step of the algorithm. */
  def extendProperties(mappingSubs: Set[Axiom], log: Log): S2STry[Set[Axiom]] =
    for 
      p <- input.pattern
      t <- input.template
      props = {
        log.profileStart("build-properties")
        val props = PropertySubsumption(p, mappingSubs, t).axioms
        log.debug("RS(q)", props)
        log.profileEnd("build-properties")
        props
      }
    yield props

  /** Add input query and shapes to log. */
  private def logInput(q: SCCQ, s: Set[SHACLShape], log: Log): Unit =
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

  private def convert(axioms: Set[Axiom])(implicit scopes: Scopes): S2STry[Set[SHACLShape]] = 
    // Make shapes from candidates over (input scope) vocabulary of query.
    for 
      t <- input.pattern
      canGen = CandidateGenerator(
        t.vocabulary,
        heuristic = ShapeHeuristic(
          optimize = false,
          proxyFamily = true,
          simpleShapes = true)
      )(scopes)
      result = {
        var result: S2STry[Set[SHACLShape]] = Right(Set())
        var all: Set[SHACLShape] = Set()
        var current = canGen.getNext().map(_.inScope(Scope.In))

        while current.nonEmpty do
          val previous =
            filter(current, AxiomSet(axioms), Log(), config.retry, config.timeout.millis)
          result = for
            p <- previous
            r <- result
          yield r.union(p)
          all = all.union(current)
          current = canGen.getNext(previous)
        // TODO: Fix me. This is not safe (might be empty).
        result.toOption.get 
      }
    yield result

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
    //t.stop()

    if result.isDefined then log.info("S_out", result.get.map(_.show))
    result.map((log, _))
