package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.dl.Equality
import de.pseifer.shar.dl.Subsumption
import de.pseifer.shar.reasoning._

import org.softlang.s2s.core._
import org.softlang.s2s.core.{vocabulary => axiomVocabulary}
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.query.AtomicPatterns
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary

import scala.concurrent.duration.*

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
):

  import shar._

  implicit val scopes: Scopes = input.composeScopes

  /** Build axioms for the query pattern. */
  def buildAxiomsPattern(pattern: AtomicPatterns, log: Log): Axioms = 
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
    Axioms(
      shapeProps
        .union(dcaP)
        .union(cwaP),
      scopes)

  /** Process the query pattern. */
  def processPattern(log: Log): S2STry[Axioms] = 
    for 
      p <- input.pattern
    yield buildAxiomsPattern(p, log)

  /** Build axioms for the query pattern. */
  def buildAxiomsTemplate(template: AtomicPatterns, pattern: AtomicPatterns, log: Log): Axioms = 

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
    Axioms(dcaH.union(cwaH).union(una), scopes)

  /** Process the query template. */
  def processTemplate(extraClauses: Set[GCORE.SetClause], log: Log): S2STry[Axioms] = 
    for 
      t <- input.template(extraClauses)
      p <- input.pattern
    yield buildAxiomsTemplate(
      template = t, 
      pattern = p, 
      log)

  /** Run the algorithm, obtaining a full set of shapes. */
  def shapes: S2STry[Set[SHACLShape]] =
    log.profileStart("candidates")
    log.profileStart("filter")

    val shapes = for 
      // First, construct all axioms.
      extraClausesAxioms <- axiomsInternal()
      extraClauses = extraClausesAxioms._1
      axioms = extraClausesAxioms._2
      // Generate candidate shapes from template.
      t <- input.template(extraClauses)
      canGen = CandidateGenerator(
        t.vocabulary,
        heuristic = config.shapeHeuristic
      )(scopes)
      // Filter candidates, checking entailment with axioms.
      result <- {
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

        log.candidates(all)
        result
      }
    yield result

    log.profileEnd("candidates")
    log.profileEnd("filter")
    log.profileEnd("algorithm")

    shapes

  /** Run algorithm, returning only the set of axioms. */
  def axioms: S2STry[Axioms] = axiomsInternal().map(_._2)

  /** Run algorithm, returning a set of axioms. */
  private def axiomsInternal(): S2STry[(Set[GCORE.SetClause], Axioms)] =

    log.profileStart("algorithm")
    input.log(log)

    log.profileStart("build")

    val axioms = for
      // Infer axioms from the query pattern.
      patternAxioms <- processPattern(log)
      // Transform inout shapes (or axioms) to axioms.
      shapeAxioms = input.shapeAxioms
      // Generate axioms from mapping components, using previous axioms.
      mappingSubs <- extendMapping(shapeAxioms.join(patternAxioms), log)
      // Generate additional clauses for GCORE queries, persisting labels and properties.
      extraClauses <- extendConstruct(patternAxioms.join(shapeAxioms).join(mappingSubs), log)
      // Generate axioms from template.
      templateAxioms <- processTemplate(extraClauses, log)
      // Generate axioms for properties.
      props <- extendProperties(mappingSubs, extraClauses, log)
      // Finally, join all axioms inferred here.
      axioms = patternAxioms
        .join(shapeAxioms)
        .join(mappingSubs)
        .join(templateAxioms)
        .join(props)
    yield (extraClauses, axioms)

    log.profileEnd("build")

    axioms

  /** Generate additional axioms using the component mapping approach. */
  def extendMapping(patternShapeAxioms: Axioms, log: Log): S2STry[Axioms] =
    for 
      // Get shapes from input, or construt using pre-existing axioms.
      shapes <- input.extensionShapes(() => convert(patternShapeAxioms))
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
    yield Axioms(mappingSubs, scopes)

  /** Generate additional axioms from property subsumptions. */
  def extendProperties(mappingSubs: Axioms, extraClauses: Set[GCORE.SetClause], log: Log): S2STry[Axioms] =
    for 
      p <- input.pattern
      t <- input.template(extraClauses)
      props = {
        log.profileStart("build-properties")
        val props = PropertySubsumption(p, mappingSubs.toSet, t).axioms
        log.debug("RS(q)", props)
        log.profileEnd("build-properties")
        props
      }
    yield Axioms(props, scopes)

  /** Extend construct (set clauses), if GCORE query. */
  def extendConstruct(axioms: Axioms, log: Log): S2STry[Set[GCORE.SetClause]] = input match
    case AlgorithmInput.SCCQAxioms(_, _) => Right(Set())
    case AlgorithmInput.SCCQSimpleSHACL(_, _, _) => Right(Set())
    case AlgorithmInput.GCOREAxioms(q, _) => 
      for 
        t <- input.pattern
        sv = input.shapeAxioms.vocabulary
        cand = generateExtensionCandidates(t.vocabulary, sv)
        f <- filter(cand, axioms, log, config.retry, config.timeout.millis)
        r <- S2SError.sequence(f.map(GCORE.shapeToSetClause(_)))
      yield r

  /** Generate set-clause validating shapes. */
  private def generateExtensionCandidates(pvoc: Vocabulary, svoc: Vocabulary): Set[SHACLShape] = 
    val thevoc = svoc.diff(pvoc)
    val theconcepts = thevoc.concepts
    val thevars = pvoc.variables
    for 
      v <- thevars
      c <- theconcepts
    yield SHACLShape(Subsumption(v.asConcept, c))

  /** Add input query and shapes to log. */
  //private def logInput(q: SCCQ, s: Set[SHACLShape], log: Log): Unit =
  //  log.info("q", q.show)
  //  log.debug("Σ(q)", q.vocabulary.show)
  //  log.info("S_in", s.map(_.show).toList)

  private def convert(axioms: Axioms)(implicit scopes: Scopes): S2STry[Set[SHACLShape]] = 
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
            filter(current, axioms, Log(), config.retry, config.timeout.millis)
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
      axioms: Axioms,
      log: Log,
      retry: Int,
      timeout: Duration,
      currentTry: Int = 1
  ): S2STry[Set[SHACLShape]] =

    // A fresh log.
    val pLog = Log(debugging = config.debug)
    val result = filterWithTimeout(candidates, axioms, pLog, timeout)

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
      axioms: Axioms,
      log: Log,
      timeout: Duration
  ): Option[(Log, Set[SHACLShape])] =

    var result: Option[Set[SHACLShape]] = None

    import java.time.LocalDateTime

    val t = new Thread {
      override def run(): Unit =
        result = Some(candidates.filter(si => axioms.entails(config)(si.axiom)))
    }

    t.start()
    t.join(timeout.toMillis)
    //t.stop()

    if result.isDefined then log.info("S_out", result.get.map(_.show))
    result.map((log, _))

