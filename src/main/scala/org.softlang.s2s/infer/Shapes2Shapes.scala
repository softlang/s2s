package org.softlang.s2s.infer

import de.pseifer.shar.Shar
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.dl.Equality
import de.pseifer.shar.dl.Subsumption
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
  implicit val scopes: Scopes = Scopes(config.renameToken)

  /** Create a fresh log. */
  private def createLog = Log(debugging = config.debug)

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
  ): S2STry[Set[SimpleSHACLShape]] =

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
      do o.map(_.show).toList.sorted.foreach(println)

    output

  /** Run validation and return the Log. */
  def validate(
      query: String,
      shapes: Set[String]
  ): (S2STry[Set[SimpleSHACLShape]], Log) =

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
  def parseQuery(query: String): S2STry[SCCQ] =
    for
      qi <- sccqp.parse(query)
      q <- SCCQ.validate(qi, config.renameToken)
    yield q

  /** The shape parser. */
  private val shapep = ShapeParser(shar)

  /** Attempt to parse a set of Simple SHACL shapes. */
  def parseShapes(
      shapes: Set[String]
  ): S2STry[Set[SimpleSHACLShape]] =
    for s <- Util
        .flipEitherHead(shapes.map(shapep.parse(_)).toList)
        .map(_.toSet)
    yield s

  /** Remove any scope-related renaming. */
  protected def descope(shapes: Set[SimpleSHACLShape]): Set[SimpleSHACLShape] =
    shapes.map(_.dropScope)

  /** Run algorithm with formal input (given a log). */
  def algorithm(
      qi: SCCQ,
      si: Set[SimpleSHACLShape],
      log: Log
  ): S2STry[Set[SimpleSHACLShape]] =

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

    val mappingSubs = SubsumptionsFromMappings(q.pattern, s).axioms
    log.debug("MA(S_in, q.P)", mappingSubs)

    log.profileEnd("build-mapping")
    log.profileStart("build-properties")

    // Add property subsumptions within query, and between query and shapes.

    val props = PropertySubsumption(q.pattern, mappingSubs, q.template).axioms
    log.debug("RS(q)", props)

    val shapeProps = ShapePropertySubsumption(q.pattern, s).axioms
    log.debug("RS(q, S_in)", shapeProps)

    log.profileEnd("build-properties")
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

    val cwaP = ClosedPropertyAssumption(q.pattern, Scope.Pattern).axioms
    log.debug("CWA(q.P), step 4.", cwaP)

    log.profileEnd("build-cwa-p")
    log.profileStart("build-cwa-t")

    // CWA for query template.

    val cwaH = ClosedPropertyAssumption(q.template, Scope.Template).axioms
    log.debug("CWA(q.H), step 5.", cwaH)

    log.profileEnd("build-cwa-t")
    log.profileStart("build-una-p")

    // UNA for query template & pattern.

    val una = UniqueNameAssumption(q.template.union(q.pattern)).axioms
    log.debug("UNA(q)", una)

    log.profileEnd("build-una")
    log.profileStart("build-top")

    // Return the complete set of axioms.

    AxiomSet(
      s.map(_.axiom)
        .union(mappingSubs)
        .union(props)
        .union(shapeProps)
        .union(dcaP)
        .union(dcaH)
        .union(cwaP)
        .union(cwaH)
        .union(una)
    )

  /** Perform the candidate generation step of the algorithm. */
  private def generateCandidates(
      q: SCCQ,
      log: Log
  ): Set[SimpleSHACLShape] =
    val cand = CandidateGenerator(
      q.template.vocabulary,
      optimize = config.optimizeCandidates,
      proxyFamily = config.proxyFamily
    )(scopes).axioms

    log.debug("S_can", cand.map(_.show))
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
  ): S2STry[Set[SimpleSHACLShape]] =

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

    if result.isDefined then log.info("S_out", result.get.map(_.show))
    result.map((log, _))
