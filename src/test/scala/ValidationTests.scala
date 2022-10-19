package org.softlang.shass.test

import org.junit.Assert.*

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import org.softlang.shass.Shass
import org.softlang.shass.core.SimpleSHACLShape

abstract class ValidationTests:

  val shass = Shass(log=true, debug=true, prefix=":", hidecolon = true)

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
    s.map("  " ++ _.show(shass.shar.state)).mkString("\n")


  /** Defines a test case. */
  def test(
      sin: Set[String],
      q: String,
      sout: Set[String],
      debug: Boolean = false
  ): Unit =

    val (actuallSout, log) = shass.validate(q, sin)
    val testSout = shass.parseShapes(sout)

    // Print full log if failure.
    for
      tout <- testSout
      aout <- actuallSout
      b = tout != aout
    do if debug || b then 
      log.print(hidecolon=true)
      if b then
        println("Obtained, in addition:\n" ++ formatResults(aout.diff(tout)))
        println("Expected, in addition:\n" ++ formatResults(tout.diff(aout)))

    // Parse error assertion.
    assert(testSout.isRight)
    assert(actuallSout.isRight)

    // Test Assertion.
    for
      tout <- testSout
      aout <- actuallSout
    do assert(tout == aout)


end ValidationTests
