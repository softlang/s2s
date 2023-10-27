package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.rules.TestName
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.infer.Shapes2Shapes
import org.stringtemplate.v4.compiler.STParser.notConditional_return

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import Console.{GREEN, RED, RESET, YELLOW, RED_B, WHITE}

abstract class ValidationTestSuite(
    // Name of the test suite.
    suite: String,
    // Do not run this test suite.
    disabled: Boolean = false,
    // Always print full debugging for failures.
    verbose: Boolean = false,
    // Disable (success) output.
    silent: Boolean = true
) extends Shapes2Shapes(
      Configuration.default.copy(
        reasoner = ActiveReasoner.Hermit
      )
    ):

  def name: TestName

  /** Empty set of shapes. */
  def noshapes: Set[String] = Set()

  /** A query with identical pattern and template. */
  def query(pattern: String): String =
    s"""
    CONSTRUCT {
      $pattern
    } WHERE {
      $pattern
    }
    """

  /** A simple query from just the pattern and template. */
  def query(template: String, pattern: String): String =
    s"""
    CONSTRUCT {
      $template
    } WHERE {
      $pattern
    }
    """

  private def formatResults(s: Set[SHACLShape]): String =
    s.map("  " ++ _.show(shar.state)).mkString("\n")

  private def leading(s: String, width: Int): String =
    val i = s.size
    List.fill(width - i)(" ").mkString("") ++ s

  private def trailing(s: String, width: Int): String =
    val i = s.size
    s ++ List.fill(width - i)(" ").mkString("")

  private def formatName(success: Boolean, info: Boolean): String =
    val ss = name.getMethodName().split("_")
    val nn = suite.take(2)
    val c = leading(ss(1), 2)
    val sc = trailing(ss(2), 1)
    val color = if success then GREEN else WHITE ++ RED_B
    val msg =
      if !success then s"${RED}failed${RESET}:"
      else if info then
        s"${GREEN}passed${RESET},${YELLOW} debugging info${RESET}:"
      else s"${GREEN}passed${RESET}."
    s"Test |${color} $c.$sc.$nn ${RESET}| in suite $suite ${msg}"

  /** Defines a test case. */
  def test(
      sin: Set[String],
      q: String,
      exactly: Set[String] = Set(),
      atleast: Set[String] = Set(),
      not: Set[String] = Set(),
      debug: Boolean = false
  ): Unit =

    // Do not run test, if the suite is disabled.
    if disabled then return

    val (actualSOutS, log) = validate(q, sin)

    // Remove internal scope.
    val actualSOut = actualSOutS.map(descope)

    // Parse the test case (and move T to Scope.Template).
    val exactlyOut = parseShapes(exactly)
    val atleastOut = parseShapes(atleast)
    val notOut = parseShapes(not)

    val success = for
      e <- exactlyOut
      a <- atleastOut
      n <- notOut
      aout <- actualSOut
    yield
      if a.isEmpty && n.isEmpty then aout == e
      else aout.intersect(n).isEmpty && a.diff(aout).isEmpty

    // Print full log if failure.
    for
      e <- exactlyOut
      a <- atleastOut
      n <- notOut
      aout <- actualSOut
      b <- success
    do
      // If failure or debugging enabled...
      if debug || !b then
        // The test failed, print error info:
        if !b then println(formatName(false, false))
        // No failure, just debugging:
        else println(formatName(true, true))

        // Now, print info about this test case / results.
        if verbose || debug then
          log.print(hidecolon = true)
          val ob = aout.diff(e.union(a))
          if ob.nonEmpty then
            println("Obtained unexpectedly:\n" ++ formatResults(ob))
          val fo = n.intersect(aout)
          if fo.nonEmpty then
            println("Obtained, even though forbidden:\n" ++ formatResults(fo))
          val mi = e.union(a).diff(aout)
          if mi.nonEmpty then println("Missing results:\n" ++ formatResults(mi))
      else
        // Successful test:
        if !silent then println(formatName(true, false))

    // Parsing and input error assertions.
    assert(exactlyOut.isRight)
    assert(atleastOut.isRight)
    assert(notOut.isRight)
    assert(actualSOut.isRight)

    // Test Assertion.
    for b <- success do assert(b == true)

end ValidationTestSuite
