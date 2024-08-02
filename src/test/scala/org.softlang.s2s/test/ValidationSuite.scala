package org.softlang.s2s.test

import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.ShapeHeuristic
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.infer.Algorithm

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import Console.{GREEN, RED, RESET, YELLOW, RED_B, WHITE}
import org.softlang.s2s.parser.JsonLDParser
import org.softlang.s2s.infer.AlgorithmInput
import org.softlang.s2s.core.dropScope
import org.softlang.s2s.core.Log
import org.softlang.s2s.core.Scopes


/** Trait for the high-level validation suite API around FunSuite. */
abstract class ValidationSuite(
  title: String = "",
  disabled: Boolean = false,
  verbose: Boolean = true,
  generateValidation: Boolean = true
) extends munit.FunSuite:

  private val validation = ValidationS2S(verbose, generateValidation, ShapeHeuristic.SimpleShapes())
  private val validationExt = ValidationS2S(
    verbose,
    generateValidation,
    ShapeHeuristic.MediumProGS(depth = 1, breadth = 2))

  /** Defines a test case checking for inclusion in output shapes. */
  def includes(
      // Name of the test case.
      description: String,
      // Input shapes.
      sin: Set[String],
      // Input query (SPARQL or G-CORE).
      q: String,
      // Output must be exactly these shapes.
      exactly: Set[String] = Set(),
      // Output must at least include these shapes.
      atleast: Set[String] = Set(),
      // Output may not include these shapes.
      not: Set[String] = Set(),
      // Use a heuristic to generate candidate shapes beyond
      // SimpleSHACL; if this flag is set, then 'exactly' should
      // not be used in favor of atleast (except for tests that
      // target at the heuristic itself).
      extended: Boolean = false,
      // Enable debugging: Print report even if no issues.
      debugging: Boolean = false
  )(implicit loc: munit.Location): Unit =

    // Do not run test, if the suite is disabled.
    if disabled then return
    val name = title ++ description

    test(name) {
      // If using 'extended' flag, use different heuristic.
      if extended then
        validationExt.includes(sin, q, exactly, atleast, not, debugging, name)
      // Otherwise, use Simple SHACL shapes.
      else
        validation.includes(sin, q, exactly, atleast, not, debugging, name)
    }

  /** Defines a test case checking for entailment in output KB. */
  def entails(
      // Name of the test case.
      description: String,
      // Input shapes.
      sin: Set[String],
      // Input query (SPARQL or G-CORE).
      q: String,
      // Output must entail these shapes.
      entails: Set[String] = Set(),
      // Output may not entail these shapes.
      not: Set[String] = Set(),
      // Enable debugging: Print report even if no issues.
      debugging: Boolean = false
  )(implicit loc: munit.Location): Unit =

    // Do not run test, if the suite is disabled.
    if disabled then return
    val name = title ++ description

    test(name) {
      validation.entails(sin, q, entails, not, debugging, name)
    }

  /** Empty set of shapes. */
  def noshapes: Set[String] = Set()

  /** A SCCQ query with identical pattern and template. */
  def sccq(pattern: String): String =
    s"""
    CONSTRUCT {
      $pattern
    } WHERE {
      $pattern
    }
    """

  /** A simple SCCQ query from just the pattern and template. */
  def sccq(template: String, pattern: String): String =
    s"""
    CONSTRUCT {
      $template
    } WHERE {
      $pattern
    }
    """

  /** A SCCQ query with identical pattern and template. */
  def query(pattern: String): String = sccq(pattern)

  /** A simple SCCQ query from just the pattern and template. */
  def query(template: String, pattern: String): String = sccq(template, pattern)

  /** A GCORE query from just the pattern and template. */
  def gcore(
      construct: String,
      matc: String,
      set: String = "",
      remove: String = "",
      where: String = ""): String =
    List(
      s"CONSTRUCT $construct",
      if set != "" then s"SET $set" else "",
      if remove != "" then s"REMOVE $remove" else "",
      s"MATCH $matc",
      if where != "" then s"WHERE $where" else ""
    ).mkString(" ")

/** ValidationSuite that extends Shapes2Shapes for testing. */
class ValidationS2S(
    // Always print full debugging for failures.
    verbose: Boolean,
    // Generate external validation output.
    generateValidation: Boolean,
    // Heuristic to use.
    heuristic: ShapeHeuristic,
) extends Shapes2Shapes(
      Configuration.default.copy(
        reasoner = ActiveReasoner.Hermit,
        shapeHeuristic = heuristic
      )
    ):

  private val dataPath = System.getProperty("user.dir") ++ "/validation/data/"

  private def formatResults(s: Set[SHACLShape]): String =
    s.map("  " ++ _.show(shar.state)).mkString("\n")

  /** Run an inclusion test case. */
  def includes(
      sin: Set[String],
      q: String,
      exactly: Set[String] = Set(),
      atleast: Set[String] = Set(),
      not: Set[String] = Set(),
      debugging: Boolean,
      name: String,
      suppressValidation: Boolean = false
  )(implicit loc: munit.Location): Unit =

    // Obtain the test result and log.
    val (actualSOutS, log) = constructShapesAndInput(q, sin)

    // Remove internal scope.
    val actualSOut = actualSOutS.map(sa => descope(sa._1))

    // Parse the test case (and move T to Scope.Template).
    val exactlyOut = parseSHACLShapes(exactly)
    val atleastOut = parseSHACLShapes(atleast)
    val notOut = parseSHACLShapes(not)

    // Parsing and input error assertions.
    // (Only for detecting errors in tests early.)
    assert(exactlyOut.isRight, "error in test case: exactlyOut")
    assert(atleastOut.isRight, "error in test case: atLeastOut")
    assert(notOut.isRight, "error in test case: notOut")

    // Assert that no internal failure occurred.
    assert(actualSOut.isRight, "internal failure")

    // If enabled, generate data for external method validation tooling.

    if !suppressValidation && generateValidation then
      generateValidationData(
        // AlgorithmInput
        actualSOutS.toOption.get._2,
        // Output shapes (expected and actual).
        // For succeeding cases: exactly/atleast are subset of actual.
        // For failing cases: Includes their union; requires investigation.
        exactlyOut.toOption.get
          .union(atleastOut.toOption.get)
          .union(actualSOut.toOption.get),
        // Name for the test.
        name,
        // The Log.
        log)

    val success = for
      e <- exactlyOut
      a <- atleastOut
      n <- notOut
      aout <- actualSOut
    yield
      // Build clue error message.
      val ob = aout.diff(e.union(a))
      val fo = n.intersect(aout)
      val mi = e.union(a).diff(aout)
      val msg = List(
        (true, "\n"),
        (true, log.format(hidecolon = true)),
        (ob.nonEmpty, s"Obtained unexpectedly:\n${RED}${formatResults(ob)}${RESET}"),
        (fo.nonEmpty, s"Obtained, even though forbidden:\n${RED}${formatResults(fo)}${RESET}"),
        (mi.nonEmpty, s"Missing results:\n${RED}${formatResults(mi)}${RESET}"),
        (true, "\n")
      ).filter(_._1).map(_._2).mkString("")

      // If neither subset of negative samples are given, check with exactlyOut.
      // Note: Using == instead of assertEquals, since diff is calculated in
      // 's' to be more readable (using Show instance for SHACLShapes).
      if a.isEmpty && n.isEmpty then
        val t = e == aout
        assert(t, msg)
        t
      // Otherwise, use both the given subset and not allowed shapes.
      else
        val t = aout.intersect(n).isEmpty && a.diff(aout).isEmpty
        assert(t, msg)
        t

    // Print debugging info if success or failure but verbose is not set; use shardikMode if explicitly debugging.
    if !verbose || success.getOrElse(false) && debugging then log.print(true, true, true, shardikMode=debugging)

  /** Run an entailment test case. */
  def entails(
      sin: Set[String],
      q: String,
      entails: Set[String] = Set(),
      not: Set[String] = Set(),
      debugging: Boolean,
      name: String,
      suppressValidation: Boolean = false
  )(implicit loc: munit.Location): Unit =

    // // Obtain the test result and log.
    val (axiomsS, log) = constructAxiomsAndInput(q, sin)

    // Assert that no internal failure occurred.
    assert(axiomsS.isRight, "internal failure")
    val axioms = axiomsS.toOption.get._1

    // Parse the test case (and move T to Scope.Template).
    val entailsOut =
      parseSHACLShapes(entails).map(s => s.map(_.inScope(Scope.Out)(axioms.scopes)))
    val notOut =
      parseSHACLShapes(not).map(s => s.map(_.inScope(Scope.Out)(axioms.scopes)))

    // Parsing and input error assertions.
    // (Only for detecting errors in tests early.)
    assert(entailsOut.isRight, "error in test case: exactlyOut")
    assert(notOut.isRight, "error in test case: notOut")

    val success = for
      e <- entailsOut
      n <- notOut
    yield
      // Take the 'entailsOut' shapes from the test case as candidates,
      // and apply the algorithm filtering step, using the inferred axioms.
      implicit val scopes = Scopes.default("-")
      val out = Algorithm.filter(e.union(n), axioms, Log())(scopes, shar)
      assert(out.isRight, "internal failure (filter)")

      // Assert that exactly all virtual candidates were entailed.
      val t1 = out.toOption.get == e
      assert(t1, "not all shapes were entailed")

      // Assert, that none of the notOut axioms were entailed.
      val t2 = out.toOption.get.intersect(n).isEmpty
      assert(t2, "shapes were entailed, that should not have been")

      t1 && t2

    // Print debugging info if success or failure but verbose is not set, shardik if explicitly debugging.
    if !verbose || success.getOrElse(false) && debugging then log.print(true, true, true, shardikMode=debugging)

    // If enabled, generate data for external method validation tooling.
    // Note: Here, we use the 'expected' shapes, since we only infer axioms.
    // This validation case is thus only valid if this test case passes, and
    // we only produce the validation output in this case.
    if !suppressValidation && generateValidation && success.getOrElse(false) then
      generateValidationData(axiomsS.toOption.get._2, entailsOut.toOption.get, name, log)

  def generateValidationData(input: AlgorithmInput, output: Set[SHACLShape], name: String, log: Log): Unit =
      // Produce all validation data.
      val query = input.formatQuery(shar.state)
      val sin = input.formatShapes.toOption.get
      val sout = JsonLDParser.unparse(output.map(_.dropScope(input.getScopes))).toOption.get
      val shardikKB = log.format(false, false, false, true)

      val cvoc = input.vocabularyIn.concepts
        .map(_.dropScope(input.getScopes)).mkString("\n").filterNot(c => c == '<' || c == '>')
      val pvoc = input.vocabularyIn.properties
        .map(_.dropScope(input.getScopes)).mkString("\n").filterNot(c => c == '<' || c == '>')
      val ivoc = input.vocabularyIn.nominals
        .map(_.dropScope(input.getScopes)).mkString("\n").filterNot(c => c == '<' || c == '>')

      val (subdir, qname) =
        if input.isECCQ
        then ("eccq/", "query.gcore")
        else ("sccq/", "query.sparql")

      // The base path of the 'validation' directory.
      val base = dataPath ++ subdir ++ ValidationS2S.TestId.next(name)

      // Query.
      val qfile = Paths.get(base ++ "/" ++ qname)
      Files.createDirectories(qfile.getParent())
      Files.write(qfile, query.getBytes(StandardCharsets.UTF_8))

      // Input shapes.
      val isfile = Paths.get(base ++ "/in.json")
      Files.write(isfile, sin.getBytes(StandardCharsets.UTF_8))

      // Output shapes.
      val osfile = Paths.get(base ++ "/out.json")
      Files.write(osfile, sout.getBytes(StandardCharsets.UTF_8))

      // Vocabularies.
      val cvfile = Paths.get(base ++ "/concepts.vocabulary")
      Files.write(cvfile, cvoc.getBytes(StandardCharsets.UTF_8))

      val pvfile = Paths.get(base ++ "/properties.vocabulary")
      Files.write(pvfile, pvoc.getBytes(StandardCharsets.UTF_8))

      val ivfile = Paths.get(base ++ "/nominals.vocabulary")
      Files.write(ivfile, ivoc.getBytes(StandardCharsets.UTF_8))

      // Executable shardik KB.
      val kbfile = Paths.get(base ++ "/shardik.kb")
      Files.write(kbfile, shardikKB.getBytes(StandardCharsets.UTF_8))

object ValidationS2S:
  private object TestId:
      private var count: Map[String, Int] = Map()
      /** Make a unique name from actual test name and running ID. */
      def next(name: String): String =
        val c = count.getOrElse(name, 0)
        count = count + (name -> (c + 1))
        if c == 0 then s"${name}"
        else s"${name}_${c}"
