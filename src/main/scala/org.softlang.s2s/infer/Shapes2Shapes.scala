package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.reasoning._
import org.softlang.s2s.core._
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.parser.ShapeParser
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary

import scala.concurrent.duration.*

import uk.ac.manchester.cs.jfact.JFactFactory
import openllet.owlapi.OpenlletReasonerFactory
import org.antlr.v4.parse.ANTLRParser.finallyClause_return

/** Customizable implementation of the S2S algorithm. */
class Shapes2Shapes(config: Configuration = Configuration.default):

  // Instanziate the reasoning framework.
  val shar = Shar()
  import shar._

  // Set the s2s prefix.
  for
    p <- Prefix.fromString(config.prefix)
    i <- Iri.fromString("<https://github.com/softlang/s2s/>")
  do shar.state.prefixes.add(p, i)

  // The scope encoding (implicit) needed for renaming.
  implicit val scopes: Scopes =
    Scopes(config.renameToken, config.namespacedTopName)

  /** Create a fresh log. */
  private def createLog: Log =
    Log(debugging = config.debug, topToken = config.namespacedTopName)

  /** Create a fresh reasoner. */
  private def createReasoner: DLReasoner =
    if config.activeReasoner == ActiveReasoner.Jfact then
      OwlApiReasoner(JFactFactory())
    else if config.activeReasoner == ActiveReasoner.Openllet then
      OwlApiReasoner(OpenlletReasonerFactory())
    else
      HermitReasoner(
        configuration = HermitConfiguration(
          existentialStrategy = HermitExistentialStrategy.IndividualReuse
        )
      )

  /** Run validation and format logging results and output (if enabled). */
  def run(
      query: String,
      shapes: Set[String]
  ): ShassTry[Set[SimpleSHACLShape]] =

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
      do o.map(_.show).foreach(println)

    output

  /** Run validation and return the Log. */
  def validate(
      query: String,
      shapes: Set[String]
  ): (ShassTry[Set[SimpleSHACLShape]], Log) =

    // Initialize the log.
    val log: Log = createLog

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

  /** The query parser. */
  private val sccqp = SCCQParser(shar)

  /** Attempt to parse a SCCQ query. */
  def parseQuery(query: String): ShassTry[SCCQ] =
    for
      qi <- sccqp.parse(query)
      q <- SCCQ.validate(
        qi,
        rename = true,
        config.renameToken,
        config.namespacedTopName
      )
    yield q

  /** The shape parser. */
  private val shapep = ShapeParser(shar)

  /** Attempt to parse a set of Simple SHACL shapes. */
  def parseShapes(
      shapes: Set[String]
  ): ShassTry[Set[SimpleSHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapep.parse(_)).toList)
        .map(_.toSet)
    yield s

  /** Remove any scope-related renaming. */
  protected def descope(shapes: Set[SimpleSHACLShape]): Set[SimpleSHACLShape] =
    shapes.map(_.dropScope).map(scopes.restoreTop)

  /** Run algorithm with formal input (given a log). */
  def algorithm(
      qi: SCCQ,
      si: Set[SimpleSHACLShape],
      log: Log
  ): ShassTry[Set[SimpleSHACLShape]] =

    log.profileStart("algorithm")

    // Pre-process query and shapes, by setting scopes.
    val preq = SCCQ(
      qi.template.inScope(Scope.Template),
      qi.pattern.inScope(Scope.Pattern)
    )
    val pres = si.map(_.inScope(Scope.Input))

    // Log input.
    logInput(preq, pres, log)
    log.profileStart("build")

    // Infer axioms from query and shapes.
    val axioms = buildAxioms(preq, pres, log)

    log.profileEnd("build")
    log.profileStart("candidates")

    // Generate candidates.
    val cand = generateCandidates(preq, log)

    log.candidates(cand)

    log.profileEnd("candidates")
    log.profileStart("filter")

    // Filter candidates.
    val result = filter(
      cand,
      axioms,
      log,
      config.retry,
      config.timeout.millis
    )

    log.profileEnd("filter")
    log.profileEnd("algorithm")

    result

  /** Add input query and shapes to log. */
  private def logInput(q: SCCQ, s: Set[SimpleSHACLShape], log: Log): Unit =
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

  /** Perform the KB construction set of the algorithm. */
  private def buildAxioms(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): AxiomSet =

    log.profileStart("build-mapping")

    // Axioms from mapping method.

    val mappingSubs =
      if config.useMappingMethod then
        SubsumptionsFromMappings(q.pattern, s).axioms
      else Set()

    if config.useMappingMethod then log.debug("Map(q.P)", mappingSubs)

    log.profileEnd("build-mapping")
    log.profileStart("build-properties")

    // Add property subsumptions within query, and between query and shapes.

    val props =
      if config.addPropertySubsumptions then
        PropertySubsumption(q.pattern, mappingSubs, q.template).axioms
      else Set()

    if config.addPropertySubsumptions then log.debug("Prop(q)", props)

    val shapeProps =
      if config.addPropertySubsumptions then
        ShapePropertySubsumption(q.pattern, s).axioms
      else Set()

    if config.addPropertySubsumptions then log.debug("Prop(q,s)", shapeProps)

    log.profileEnd("build-properties")
    log.profileStart("build-dca-p")

    // DCA for query pattern.

    val dcaP =
      if config.dcaForPattern then
        DomainClosureAssumptionForPattern(
          q.pattern,
          eraseVariables = config.erasePvariables,
          approximateVariables = config.approximatePvariables,
          useSubsumption = config.useSubsumptionInPatternDCA,
          includeConceptClosure = config.includeConceptClosurePattern,
          includeVariableClosure = config.includeVariableClosurePattern
        ).axioms
      else Set()

    if config.dcaForPattern then log.debug("DCA(q.P)", dcaP)

    log.profileEnd("build-dca-p")
    log.profileStart("build-dca-t")

    // DCA for query template.

    val dcaH =
      if config.dcaForTemplate then
        DomainClosureAssumptionForTemplate(
          q.template,
          eraseVariables = config.eraseHvariables,
          approximateVariables = config.approximateHvariables,
          useSubsumption = config.useSubsumptionInTemplateDCA,
          includeConceptClosure = config.includeConceptClosureTemplate,
          includeVariableClosure = config.includeVariableClosureTemplate
        ).axioms
      else Set()

    if config.dcaForTemplate then log.debug("DCA(q.H)", dcaH)

    log.profileEnd("build-dca-t")
    log.profileStart("build-cwa-p")

    // CWA for query pattern.

    val cwaP =
      if config.cwaForPattern then
        if config.alternativeCWA then
          AlternativeClosedWorldAssumption(
            q.pattern,
            Scope.Pattern
          ).axioms
        else
          ClosedWorldAssumptionForPattern(
            q.pattern,
            config.closeConcepts,
            config.closeProperties,
            config.closeLiterals,
            config.useSubsumptionInPatternCWA
          ).axioms
      else Set()

    if config.cwaForPattern then log.debug("CWA(q.P)", cwaP)

    log.profileEnd("build-cwa-p")
    log.profileStart("build-cwa-t")

    // CWA for query template.

    val cwaH =
      if config.cwaForTemplate then
        if config.alternativeCWA then
          AlternativeClosedWorldAssumption(
            q.pattern,
            Scope.Template
          ).axioms
        else
          ClosedWorldAssumptionForTemplate(
            q.template,
            config.closeConcepts,
            config.closeProperties,
            config.closeLiterals,
            config.useSubsumptionInTemplateCWA
          ).axioms
      else Set()

    if config.cwaForTemplate then log.debug("CWA(q.H)", cwaH)

    log.profileEnd("build-cwa-t")
    log.profileStart("build-una-p")

    // UNA for query pattern.

    val unaP =
      if config.unaForPattern then UniqueNameAssumption(q.pattern).axioms
      else Set()

    if config.unaForPattern then log.debug("UNA(q.P)", unaP)

    log.profileEnd("build-una-p")
    log.profileStart("build-una-t")

    // UNA for query template.

    val unaH =
      if config.unaForTemplate then UniqueNameAssumption(q.template).axioms
      else Set()

    if config.unaForTemplate then log.debug("UNA(q.H)", unaH)

    log.profileEnd("build-una-t")
    log.profileStart("build-una")

    // UNA for query template & pattern.

    val una =
      if config.unaForBoth then
        UniqueNameAssumption(q.template.union(q.pattern)).axioms
      else Set()

    if config.unaForBoth then log.debug("UNA(q)", una)

    log.profileEnd("build-una")
    log.profileStart("build-top")

    // Namespaced Top definitions.

    val tops =
      NamespacedTop(q.pattern, q.template, config.useNamespacedTop).axioms
    log.debug("T", tops)

    log.profileEnd("build-top")
    log.profileStart("build-add")

    // Return the complete set of axioms.

    AxiomSet(
      s.map(_.axiom)
        .union(mappingSubs)
        .union(props)
        .union(shapeProps)
        .union(dcaP)
        .union(dcaH)
        .union(cwaP)
        .union(unaP)
        .union(cwaH)
        .union(unaH)
        .union(una)
        .union(tops)
    )

  /** Perform the candidate generation step of the algorithm. */
  private def generateCandidates(
      q: SCCQ,
      log: Log
  ): Set[SimpleSHACLShape] =
    val cand = CandidateGenerator(
      q.template.vocabulary,
      optimize = config.optimizeCandidates
    )(scopes).axioms

    log.debug("S_can", cand.map(_.show).toList)
    log.debugNoisy(s"(${cand.size})")

    cand

  /** Perform the filtering step of the algorithm. */
  private def filter(
      candidates: Set[SimpleSHACLShape],
      axioms: AxiomSet,
      log: Log,
      retry: Int,
      timeout: Duration,
      currentTry: Int = 1
  ): ShassTry[Set[SimpleSHACLShape]] =

    // A fresh log.
    val plog = createLog

    // A fresh reasoner.
    val reasoner = createReasoner

    reasoner.addAxioms(axioms)
    val result = filterWithTimeout(candidates, reasoner, plog, timeout)

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

  private def filterWithTimeout(
      candidates: Set[SimpleSHACLShape],
      reasoner: DLReasoner,
      log: Log,
      timeout: Duration
  ): Option[(Log, Set[SimpleSHACLShape])] =

    var result: Option[Set[SimpleSHACLShape]] = None

    import java.time.LocalDateTime

    val t = new Thread {
      override def run(): Unit =
        result = Some(candidates.filter(si => reasoner.prove(si.axiom)))
    }

    t.start()
    t.join(timeout.toMillis)
    t.stop()

    if result.isDefined then log.info("S_out", result.get.map(_.show).toList)
    result.map((log, _))
