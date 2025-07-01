package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.dl._
import de.pseifer.shar.reasoning._

import org.softlang.s2s.core._
import org.softlang.s2s.core.{vocabulary => axiomVocabulary}
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.query.AtomicPatterns
import org.softlang.s2s.query.GCORE
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary
import org.softlang.s2s.core.{inScope => cinScope}

import scala.concurrent.duration.*
import org.softlang.s2s.query.variables
import org.softlang.s2s.query.FilterPattern
import org.softlang.s2s.query.GCORE.Label
import org.softlang.s2s.query.GCORE.Key

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
  implicit val ishar: Shar = shar

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

    // log.debug("CWA(q.P), step 1", dcaP1)
    log.debug("CWA(q.P), step 3.", dcaP3)
    log.profileEnd("build-dca-p")

    // CWA for query pattern.

    log.profileStart("build-cwa-p")

    // val cwaP = ClosedPropertyAssumption(pattern, Scope.Med, input).axioms
    // log.debug("CWA(q.P), step 4.", cwaP)

    log.profileEnd("build-cwa-p")

    // Property subsumption.

    log.profileStart("build-properties")

    val shapeProps = ShapePropertySubsumption(pattern, input.isECCQ).axioms
    log.debug("RS(q, S_in)", shapeProps)

    log.profileEnd("build-properties")

    // Return union of components.
    Axioms(
      shapeProps
        .union(dcaP3),
      // .union(cwaP),
      scopes
    )

  /** Process the query pattern. */
  def processPattern(log: Log): S2STry[Axioms] =
    for p <- input.pattern
    yield buildAxiomsPattern(p, log)

  /** Build axioms for the query template. */
  def buildAxiomsTemplate(
      template: AtomicPatterns,
      pattern: AtomicPatterns,
      log: Log
  ): Axioms =

    // TODO ??? If this is a return query, nothing to add (EARLY RETURN).
    // if input.isRETURN then return Axioms(Set(), scopes)

    // DCA for query template.
    log.profileStart("build-dca-t")

    // TODO: "Old" variables concepts are not filtered correctly on left hand side.
    val dcaH = ClosedConceptAssumptionTemplate(template, input).axioms
    log.debug("CWA(q.H), step 2.", dcaH)

    log.profileEnd("build-dca-t")

    // CWA for query template.

    log.profileStart("build-cwa-t")

    // TODO: Must consider all names in vocabulary, not just vocabulary of H.
    val cwaH = ClosedPropertyAssumption(template, Scope.Out, input).axioms
    log.debug("CWA(q.H), step 5.", cwaH)

    log.profileEnd("build-cwa-t")

    val rule6: Set[Axiom] =
      if !input.isECCQ then Set()
      else
        val vC = input.vocabulary.concepts
          .map(_.cinScope(Scope.In))
          .map(_.asInstanceOf[NamedConcept])
        val nvC = vC.filter(c => Label.isNodeLabel(c.c))
        val evC = vC.filter(c => Label.isEdgeLabel(c.c))

        val ncs: Set[Axiom] = for
          nv <- input.nodeVariables
          c <- nvC
          lhs <- nv.asConceptComponent(input.filters, c)
        // yield Subsumption(lhs, Intersection(nv.asConcept, c))
        yield Equality(lhs, Intersection(nv.asConcept, c))

        val ecs: Set[Axiom] = for
          ev <- input.edgeVariables
          c <- evC
          lhs <- ev.asConceptComponent(input.filters, c)
        // yield Subsumption(lhs, Intersection(ev.asConcept, c))
        yield Equality(lhs, Intersection(ev.asConcept, c))

        ncs.union(ecs)

    log.debug("CWA(q.H), step 6.", rule6)

    val rule7: Set[Axiom] =
      if !input.isECCQ then Set()
      else
        val vP = input.vocabulary.properties
          .map(_.cinScope(Scope.In))
          .map(_.asInstanceOf[NamedRole])

        val nvP = vP.filter(r => Key.isNodeKey(r.r))
        val evP = vP.filter(r => Key.isEdgeKey(r.r))

        val nps: Set[Axiom] = for
          nv <- input.nodeVariables
          p <- nvP
          (lhs, o) <- nv.asRoleObjectComponent(input.filters, p)
        // yield Subsumption(lhs, Intersection(nv.asConcept, Existential(p, o)))
        yield Equality(lhs, Intersection(nv.asConcept, Existential(p, o)))

        val eps: Set[Axiom] = for
          ev <- input.edgeVariables
          p <- evP
          (lhs, o) <- ev.asRoleObjectComponent(input.filters, p)
        // yield Subsumption(lhs, Intersection(ev.asConcept, Existential(p, o)))
        yield Equality(lhs, Intersection(ev.asConcept, Existential(p, o)))

        // Explicit subsumption for shape properties.
        // or the form ∃:age_o_o.⊤ ≡ :x_age (where 'age' occurrs in shapes only).
        // TODO: Review
        val subsp: Set[Axiom] = for
          nv <- input.nodeVariables
          p <- shapeProperties.getOrElse(Set())
          (rhs, o) <- nv.asRoleObjectComponent(input.filters, p)
        yield Subsumption(Existential(p.cinScope(Scope.Out), Top), rhs)

        nps.union(eps).union(subsp)

    log.debug("CWA(q.H), step 7.", rule7)

    // UNA for query template & pattern.

    log.profileStart("build-una-p")

    val una = UniqueNameAssumption(template.concat(pattern)).axioms
    log.debug("UNA(q)", una)

    log.profileEnd("build-una")

    // Return the complete set of axioms.
    Axioms(dcaH.union(cwaH).union(rule6).union(rule7).union(una), scopes)

  /** Process the query template. */
  def processTemplate(
      /*extraClauses: Set[GCORE.SetClause],*/ log: Log
  ): S2STry[Axioms] =
    for
      t <- input.template
      p <- input.pattern
    yield buildAxiomsTemplate(template = t, pattern = p, log)

  /** Get the vocabulary for candidate generation. */
  def candidateVocabulary: Vocabulary =
    // In ECCQ queries, must consider additional names.
    if input.isECCQ then input.vocabulary.inScope(Scope.Out)
    // For SCCQ, the template vocabulary suffices.
    else input.template.map(_.vocabulary).getOrElse(Vocabulary.empty)

  /** Run the algorithm, obtaining a full set of shapes. */
  def shapes: S2STry[Set[SHACLShape]] =
    log.profileStart("candidates")
    log.profileStart("filter")

    val shapes = for
      // First, construct all axioms.
      // extraClausesAxioms <- axiomsInternal()
      // extraClauses = extraClausesAxioms._1
      axioms <- axiomsInternal()
      canGen = CandidateGenerator(
        // Generate candidate shapes from template.
        candidateVocabulary,
        heuristic = config.shapeHeuristic,
        excludeTarget = Set(GCORE.nodeToEdgeIri, GCORE.edgeToNodeIri)
      )(scopes)
      // Filter candidates, checking entailment with axioms.
      result <- {
        var result: S2STry[Set[SHACLShape]] = Right(Set())
        var all: Set[SHACLShape] = Set()
        var current = canGen.getNext()

        while current.nonEmpty do
          val previous =
            Algorithm.filter(current, axioms, log, config)
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
  def axioms: S2STry[Axioms] = axiomsInternal()

  /** Run algorithm, returning a set of axioms. */
  private def axiomsInternal(): S2STry[Axioms] =

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
      // extraClauses <- extendConstruct(patternAxioms.join(shapeAxioms).join(mappingSubs), log)
      // Generate axioms from template.
      templateAxioms <- processTemplate(log)
      // Generate axioms for properties.
      props <- extendProperties(mappingSubs, log)
      // Bonus properties for ECCQ queries (properties).
      bonusP <- bonusProperties(log)
      // Finally, join all axioms inferred here.
      axioms = patternAxioms
        .join(shapeAxioms)
        .join(mappingSubs)
        .join(templateAxioms)
        .join(props)
        .join(bonusP)
    yield axioms

    log.profileEnd("build")
    axioms

  /** Generate additional axioms using the component mapping approach. */
  def extendMapping(patternShapeAxioms: Axioms, log: Log): S2STry[Axioms] =
    // Disable mapping for extended ECCQ.
    if input.isECCQ then return Right(Axioms(Set(), scopes))
    else
      for
        // Get shapes from input, or construt using pre-existing axioms.
        shapes <- input.extensionShapes(() => convert(patternShapeAxioms))
        p <- input.pattern
        mappingSubs = {
          log.profileStart("build-mapping")
          val mappingSubs = SubsumptionsFromMappings(
            p,
            shapes.map(_.toSimple).filter(_.nonEmpty).map(_.get)
          ).axioms
          log.debug("MA(S_in, q.P)", mappingSubs)
          log.profileEnd("build-mapping")
          mappingSubs
        }
      yield Axioms(mappingSubs, scopes)

  /** Generate additional axioms from property subsumptions. */
  def extendProperties(
      mappingSubs: Axioms, /*extraClauses: Set[GCORE.SetClause],*/ log: Log
  ): S2STry[Axioms] =
    for
      p <- input.pattern
      t <- input.template
      props = {
        log.profileStart("build-properties")
        val props = PropertySubsumption(p, mappingSubs.toSet, t).axioms
        log.debug("RS(q)", props)
        log.profileEnd("build-properties")
        props
      }
    yield Axioms(props, scopes)

  /** Get all properties only mentioned in shapes. */
  def shapeProperties: S2STry[Set[NamedRole]] =
    for
      p <- input.pattern
      t <- input.template
    yield
      // Exclude names that do occur in pattern or template (handled elsewhere).
      val except = p.vocabulary.properties
        .map(_.cinScope(Scope.In))
        .union(
          t.vocabulary.properties.map(_.cinScope(Scope.In))
        )
      input.vocabulary.properties
        .map(_.cinScope(Scope.In))
        .diff(except)
        // Lost by scoping.
        .map(_.asInstanceOf[NamedRole])
        .toSet

  /** Generate additional axioms from property subsumptions for ECCQ queries. */
  def bonusProperties(log: Log): S2STry[Axioms] =
    if input.isECCQ then
      for these <- shapeProperties
      yield
        val newps: Set[Axiom] = these.flatMap { p =>
          // TODO review
          Set(
            RoleSubsumption(p.cinScope(Scope.Out), p),
            RoleSubsumption(p, p.cinScope(Scope.Out))
          )
        }
        log.debug("bProp(q)", newps)
        Axioms(newps, scopes)
    else Right(Axioms.empty(scopes))

    // for
    //   p <- input.pattern
    //   t <- input.template
    //   props = {
    //     log.profileStart("build-properties")
    //     val props = PropertySubsumption(p, mappingSubs.toSet, t).axioms
    //     log.debug("RS(q)", props)
    //     log.profileEnd("build-properties")
    //     props
    //   }
    // yield Axioms(props, scopes)

  /** Extend construct (set clauses), if GCORE query. */
  // def extendConstruct(axioms: Axioms, log: Log): S2STry[Set[GCORE.SetClause]] = input match
  //  case AlgorithmInput.SCCQAxioms(_, _) => Right(Set())
  //  case AlgorithmInput.SCCQSimpleSHACL(_, _, _) => Right(Set())
  //  case AlgorithmInput.GCOREAxioms(q, _) =>
  //    for
  //      t <- input.pattern
  //      sv = input.shapeAxioms.vocabulary
  //      cand = generateExtensionCandidates(t.vocabulary, sv)
  //      f <- filter(cand, axioms, Log(), config.retry, config.timeout.millis)
  //      r <- S2SError.sequence(f.map(GCORE.shapeToSetClause(_)))
  //    yield r

  /** Generate set-clause validating shapes. */
  // private def generateExtensionCandidates(pvoc: Vocabulary, svoc: Vocabulary): Set[SHACLShape] =
  //  val thevoc = svoc.diff(pvoc)
  //  val theconcepts = thevoc.concepts
  //  val thevars = pvoc.variables
  //  for
  //    v <- thevars
  //    c <- theconcepts
  //  yield SHACLShape(Subsumption(v.asConcept, c))

  /** Add input query and shapes to log. */
  // private def logInput(q: SCCQ, s: Set[SHACLShape], log: Log): Unit =
  //  log.info("q", q.show)
  //  log.debug("Σ(q)", q.vocabulary.show)
  //  log.info("S_in", s.map(_.show).toList)

  private def convert(axioms: Axioms)(implicit
      scopes: Scopes
  ): S2STry[Set[SHACLShape]] =
    // Make shapes from candidates over (input scope) vocabulary of query.
    for
      t <- input.pattern
      canGen = CandidateGenerator(
        t.vocabulary,
        heuristic =
          ShapeHeuristic.SimpleShapes(opt = false, proxyFamily = true),
        excludeTarget = Set(GCORE.nodeToEdgeIri, GCORE.edgeToNodeIri)
      )(scopes)
      result = {
        var result: S2STry[Set[SHACLShape]] = Right(Set())
        var all: Set[SHACLShape] = Set()
        var current = canGen.getNext().map(_.inScope(Scope.In))

        while current.nonEmpty do
          val previous =
            Algorithm.filter(current, axioms, Log(), config)
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

/** Companion object of Algorithm class, that implements some of the re-useable
  * steps of the algorithm.
  */
object Algorithm:

  /** Perform the filtering step of the algorithm. */
  def filter(
      candidates: Set[SHACLShape],
      axioms: Axioms,
      log: Log,
      config: Configuration = Configuration.default,
      currentTry: Int = 1
  )(implicit scopes: Scopes, shar: Shar): S2STry[Set[SHACLShape]] =

    // A fresh log.
    val pLog = Log(debugging = config.debug)
    val result = filterWithTimeout(config, candidates, axioms, pLog)

    // Wait configured timeout for completion.
    result match
      case Some(pr) =>
        // Include log only of the completed run / if completed.
        log.append(pr._1)
        // Return the filtered set of shapes.
        Right(pr._2)
      case None =>
        if currentTry <= config.retry then
          log.restart("filter", currentTry, config.retry, config.timeout.millis)
          filter(candidates, axioms, log, config, currentTry + 1)
        else
          log.timeout("filter", config.retry, config.timeout.millis)
          Left(TimeoutError(config.timeout, config.retry))

  /** Filter, with provided timeout value that aborts filtering. */
  private def filterWithTimeout(
      config: Configuration,
      candidates: Set[SHACLShape],
      axioms: Axioms,
      log: Log
  )(implicit scopes: Scopes, shar: Shar): Option[(Log, Set[SHACLShape])] =

    var result: Option[Set[SHACLShape]] = None

    import java.time.LocalDateTime

    val t = new Thread {
      override def run(): Unit =
        result = Some(candidates.filter(si => axioms.entails(config)(si.axiom)))
    }

    t.start()
    t.join(config.timeout.millis.toMillis)
    // t.stop()

    if result.isDefined then
      log.info("S_out", result.get.map(_.show(shar.state)))
    result.map((log, _))
