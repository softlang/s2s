package org.softlang.s2s.test

import org.junit.Assert.*

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import org.softlang.s2s.Shapes2Shapes
import org.softlang.s2s.core.{SimpleSHACLShape, Configuration}
import org.stringtemplate.v4.compiler.STParser.notConditional_return

abstract class ValidationTests:

  val s2s =
    Shapes2Shapes(
      Configuration.join(
        Configuration.philippsMethod,
        Configuration.debug,
        Configuration.formalOutput
      )
    )

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

  private def formatResults(s: Set[SimpleSHACLShape]): String =
    s.map("  " ++ _.show(s2s.shar.state)).mkString("\n")

  /** Defines a test case. */
  def test(
      sin: Set[String],
      q: String,
      exactly: Set[String] = Set(),
      atleast: Set[String] = Set(),
      not: Set[String] = Set(),
      debug: Boolean = false
  ): Unit =

    val (actuallSout, log) = s2s.validate(q, sin)

    val exactlyOut = s2s.parseShapes(exactly)
    val atleastOut = s2s.parseShapes(atleast)
    val notOut = s2s.parseShapes(not)

    val success = for
      e <- exactlyOut
      a <- atleastOut
      n <- notOut
      aout <- actuallSout
    yield
      if a.isEmpty && n.isEmpty then aout == e
      else aout.intersect(n).isEmpty && a.diff(aout).isEmpty

    // Print full log if failure.
    for
      e <- exactlyOut
      a <- atleastOut
      n <- notOut
      aout <- actuallSout
      b <- success
    do
      if debug || !b then
        log.print(hidecolon = true)
        val ob = aout.diff(e.union(a))
        if ob.nonEmpty then
          println("Obtained unexpectedly:\n" ++ formatResults(ob))
        val fo = n.intersect(aout)
        if fo.nonEmpty then
          println("Obtained, even though forbidden:\n" ++ formatResults(fo))
        val mi = e.union(a).diff(aout)
        if mi.nonEmpty then
          println("Missing from output:\n" ++ formatResults(mi))

    // Parsing and input error assertions.
    assert(exactlyOut.isRight)
    assert(atleastOut.isRight)
    assert(notOut.isRight)
    assert(actuallSout.isRight)

    // Test Assertion.
    for b <- success do assert(b == true)

end ValidationTests
