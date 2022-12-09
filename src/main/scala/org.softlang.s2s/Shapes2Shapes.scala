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
import org.softlang.s2s.query.rename
import org.softlang.s2s.query.vocabulary
import org.stringtemplate.v4.compiler.GroupParser.formalArgs_scope

/** Customizable implementation of the S2S algorithm. */
class Shapes2Shapes(config: Configuration = Configuration.default):

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
      q <- SCCQ.validate(qi, config.autoRename)
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

    // Print results (if enabled).
    if config.printOutput then
      for result <- res._1
      do result.map(_.show).foreach(println)

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
    yield algorithm(
      // Rename the inputs if required.
      if config.autoRename then
        SCCQ(q.template, q.pattern.rename(config.renameToken))
      else q,
      if config.autoRename then s.map(_.rename(config.renameToken)) else s,
      log
    )

    // Output (first) error, if any.
    sout.left.map(r => log.error(r.show))

    (sout, log)

  /** Run algorithm with formal input (given a log). */
  def algorithm(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): Set[SimpleSHACLShape] =

    // Log input.
    logInput(q, s, log)

    // Infer axioms from query and shapes.
    val hermit = buildKB(q, s, log)

    // Generate candidates.
    val cand = generateCandidates(q, log)

    // Filter candidates.
    filter(cand, hermit, log)

  /** Add input query and shapes to log. */
  def logInput(q: SCCQ, s: Set[SimpleSHACLShape], log: Log): Unit =
    log.info("q", q.show)
    log.debug("Σ(q)", q.vocabulary.show)
    log.info("S_in", s.map(_.show).toList)

  /** Perform the candidate generation step of the algorithm. */
  def generateCandidates(
      q: SCCQ,
      log: Log
  ): Set[SimpleSHACLShape] =
    val cand = CandidateGenerator(
      q.template.vocabulary,
      optimize = config.optimizeCandidates
    ).axioms

    log.debug("S_can", cand.map(_.show).toList)

    cand

  /** Perform the filtering step of the algorithm. */
  def filter(
      canditates: Set[SimpleSHACLShape],
      hermit: HermitReasoner,
      log: Log
  ): Set[SimpleSHACLShape] =
    val out = canditates.filter(si => hermit.prove(si.axiom))
    log.info("S_out", out.map(_.show).toList)
    out

  /** Perform the KB construction set of the algorithm. */
  def buildKB(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log: Log
  ): HermitReasoner =

    // Axioms from mapping method.

    val mappingSubs =
      if config.useMappingMethod then
        SubsumptionsFromMappings(q.pattern, s).axioms
      else Set()

    if config.useMappingMethod then
      log.debug("Map(q.P)", mappingSubs.map(_.show).toList)

    // Add property subsumptions within query, and between query and shapes.

    val props =
      if config.addPropertySubsumptions then
        PropertySubsumption(
          q.pattern,
          mappingSubs,
          q.template,
          renameProperties = config.renamePatternInternalProperties,
          renameToken = config.renameToken
        ).axioms
      else Set()

    if config.addPropertySubsumptions then
      log.debug("Prop(q)", props.map(_.show).toList)

    val shapeProps =
      if config.addPropertySubsumptions then
        ShapePropertySubsumption(
          q.pattern,
          s,
          renameProperties = config.renamePatternInternalProperties,
          renameToken = config.renameToken
        ).axioms
      else Set()

    if config.addPropertySubsumptions then
      log.debug("Prop(q,s)", shapeProps.map(_.show).toList)

    // DCA for query pattern.

    val dcaP =
      if config.dcaForPattern then
        DomainClosureAssumption(
          q.pattern,
          eraseVariables = config.erasePvariables,
          approximateVariables = config.approximatePvariables,
          useSubsumption = config.useSubsumptionInPatternDCA
        ).axioms
      else Set()

    if config.dcaForPattern then log.debug("DCA(q.P)", dcaP.map(_.show).toList)

    // DCA for query template.

    val dcaH =
      if config.dcaForTemplate then
        DomainClosureAssumption(
          q.template,
          eraseVariables = config.eraseHvariables,
          approximateVariables = config.approximateHvariables,
          useSubsumption = config.useSubsumptionInTemplateDCA
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
          config.closeTop,
          config.closeLiterals,
          config.useSubsumptionInPatternCWA,
          renameConcepts = config.renamePatternInternalConcepts,
          renameProperties = config.renamePatternInternalProperties,
          renameToken = config.renameToken
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
          config.closeTop,
          config.closeLiterals,
          config.useSubsumptionInTemplateCWA,
          renameConcepts = false,
          renameProperties = false,
          renameToken = config.renameToken
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
          .union(mappingSubs)
          .union(props)
          .union(shapeProps)
          .union(dcaP)
          .union(dcaH)
          .union(cwaP)
          .union(unaP)
          .union(cwaH)
          .union(unaH)
      )
    )
    hermit
