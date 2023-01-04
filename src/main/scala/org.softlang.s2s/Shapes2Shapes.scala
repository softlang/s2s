package org.softlang.s2s

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.reasoning.AxiomSet
import de.pseifer.shar.reasoning.HermitReasoner
import org.softlang.s2s.core._
import org.softlang.s2s.generate.CandidateGenerator
import org.softlang.s2s.infer._
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.parser.ShapeParser
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.inScope
import org.softlang.s2s.query.vocabulary
import org.stringtemplate.v4.compiler.GroupParser.formalArgs_scope

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

  /** Run validation and format results (if enabled). */
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

    // Print results (if enabled).
    if config.printOutput then
      for result <- resShapes
      do result.map(_.show).foreach(println)

    // Return output shapes.
    resShapes

  /** Run validation and return a Log. */
  def validate(
      query: String,
      shapes: Set[String]
  ): (ShassTry[Set[SimpleSHACLShape]], Log) =

    // Initialize the log.
    val log: Log =
      Log(debugging = config.debug, topToken = config.namespacedTopName)

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
  def parseShapes(shapes: Set[String]): ShassTry[Set[SimpleSHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapep.parse(_)).toList)
        .map(_.toSet)
    yield s

  /** Run algorithm with formal input (given a log). */
  def algorithm(
      qi: SCCQ,
      si: Set[SimpleSHACLShape],
      log: Log
  ): Set[SimpleSHACLShape] =

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
    val hermit = buildKB(preq, pres, log)

    log.profileEnd("build")
    log.profileStart("candidates")

    // Generate candidates.
    val cand = generateCandidates(preq, log)

    log.profileEnd("candidates")
    log.profileStart("filter")

    // Filter candidates.
    val result = filter(cand, hermit, log)

    log.profileEnd("filter")
    log.profileEnd("algorithm")

    result

  /** Add input query and shapes to log. */
  private def logInput(q: SCCQ, s: Set[SimpleSHACLShape], log: Log): Unit =
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

  /** Perform the KB construction set of the algorithm. */
  private def buildKB(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): HermitReasoner =

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
          useSubsumption = config.useSubsumptionInPatternDCA
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
          useSubsumption = config.useSubsumptionInTemplateDCA
        ).axioms
      else Set()

    if config.dcaForTemplate then log.debug("DCA(q.H)", dcaH)

    log.profileEnd("build-dca-t")
    log.profileStart("build-cwa-p")

    // CWA for query pattern.

    val cwaP =
      if config.cwaForPattern then
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
    log.profileStart("build-top")

    // Namespaced Top definitions.

    val tops =
      NamespacedTop(q.pattern, q.template, config.useNamespacedTop).axioms
    log.debug("T", tops)

    log.profileEnd("build-top")
    log.profileStart("build-add")

    // Initialize the reasoner.

    val hermit = HermitReasoner.default

    val axs = AxiomSet(
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
        .union(tops)
    )

    hermit.addAxioms(axs)

    log.profileStart("build-add")

    hermit

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
    cand

  /** Perform the filtering step of the algorithm. */
  private def filter(
      candidates: Set[SimpleSHACLShape],
      hermit: HermitReasoner,
      log: Log
  ): Set[SimpleSHACLShape] =

    // Process first candidate separately (for profiling).
    val first = candidates.take(1)
    val rest = candidates.drop(1)

    log.profileStart("filter-inithermit")
    val ff = first.filter(si => hermit.prove(si.axiom))
    log.profileEnd("filter-inithermit")

    print(candidates.size)

    val out = ff ++ rest.filter(si =>
      print(".")
      hermit.prove(si.axiom)
    )
    println()

    log.info("S_out", out.map(_.show).toList)
    out
