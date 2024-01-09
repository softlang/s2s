package org.softlang.s2s.test

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

/** Trait for the Validation suite that builds public API. */
abstract class ValidationSuite extends munit.FunSuite:

  /** ValidationSuite that extends Shapes2Shapes for testing. */
  private class ValidationS2S(
      // Name of the test suite.
      suite: String,
      // Do not run this test suite.
      disabled: Boolean = false,
      // Always print full debugging for failures.
      verbose: Boolean = false,
      // Disable (success) output.
      silent: Boolean = false
  ) extends Shapes2Shapes(
        Configuration.default.copy(
          reasoner = ActiveReasoner.Hermit
        )
      ):

    private def formatResults(s: Set[SHACLShape]): String =
      s.map("  " ++ _.show(shar.state)).mkString("\n")

    /** Defines a test case. */
    def work(
        sin: Set[String],
        q: String,
        exactly: Set[String] = Set(),
        atleast: Set[String] = Set(),
        not: Set[String] = Set()
    )(implicit loc: munit.Location): Unit =

      // Do not run test, if the suite is disabled.
      if disabled then return

      val (actualSOutS, log) = constructShapes(q, sin)

      // Remove internal scope.
      val actualSOut = actualSOutS.map(descope)

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

      val success = for
        e <- exactlyOut
        a <- atleastOut
        n <- notOut
        aout <- actualSOut
      do
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
        if a.isEmpty && n.isEmpty then assert(e == aout, msg)
        // Otherwise, use both the given subset and not allowed shapes.
        else assert(aout.intersect(n).isEmpty && a.diff(aout).isEmpty, msg)

    def workG(
        sin: Set[String],
        q: String,
        exactly: Set[String] = Set(),
        atleast: Set[String] = Set(),
        not: Set[String] = Set()
    )(implicit loc: munit.Location): Unit =

      /** TODO: Implement me */
      throw new NotImplementedError

      // Do not run test, if the suite is disabled.
      if disabled then return

      val (actualSOutS, log) = constructShapes(q, sin)

      // Remove internal scope.
      val actualSOut = actualSOutS.map(descope)

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

      val success = for
        e <- exactlyOut
        a <- atleastOut
        n <- notOut
        aout <- actualSOut
      do
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
        if a.isEmpty && n.isEmpty then assert(e == aout, msg)
        // Otherwise, use both the given subset and not allowed shapes.
        else assert(aout.intersect(n).isEmpty && a.diff(aout).isEmpty, msg)

  private val validation = ValidationS2S("some-name")

  /** Defines a test case. */
  def work(
      description: String,
      sin: Set[String],
      q: String,
      exactly: Set[String] = Set(),
      atleast: Set[String] = Set(),
      not: Set[String] = Set(),
      gcore: Boolean = false
  )(implicit loc: munit.Location): Unit =
    test(description) {
      if gcore then
        validation.workG(sin, q, exactly, atleast, not)
      else
        validation.work(sin, q, exactly, atleast, not)
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
  def gcore(construct: String, matc: String, set: String = "", remove: String = "", where: String = ""): String =
    s"""CONSTRUCT $construct""" +
    (if set != "" then s"""SET $set""" else "") +
    (if set != "" then s"""REMOVE $remove""" else "") +
    s"""MATCH $matc""" +
    (if set != "" then s"""WHERE $where""" else "")

