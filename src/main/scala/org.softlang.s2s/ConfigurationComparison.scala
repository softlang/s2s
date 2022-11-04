package org.softlang.s2s

import org.softlang.s2s.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.query.{SCCQ, vocabulary}

import de.pseifer.shar.dl.{Axiom, Subsumption}

import scala.util.Random
import scala.collection.BitSet

class ConfigurationComparison(
    c1: Configuration,
    c2: Configuration,
    compareVariableSubsumptions: Boolean = false,
    compareResults: Boolean = true,
    stepTrials: Int = 1000,
    randomSets: Int = 0,
    stopAfterFirstResult: Boolean = true
):

  // Test Instances.
  private val s1 = Shapes2Shapes(c1)
  private val s2 = Shapes2Shapes(c2)

  /** Compare one set of query and shapes. */
  private def compare(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log1: Log,
      log2: Log
  ): (Boolean, Boolean) =

    val kb1 = s1.buildKB(q, s, log1)
    val kb2 = s2.buildKB(q, s, log2)

    // Perform first test: Comparison of subsumption.

    val t1 =
      if compareVariableSubsumptions then

        // Construct all variable subsuptions.
        val subs: List[Axiom] = (for
          v1 <- q.variables
          v2 <- q.variables
          if v1 != v2
        yield Subsumption(v1.asConcept, v2.asConcept)).toList

        val r1 = subs.map(kb1.prove)
        val r2 = subs.map(kb2.prove)

        log1.info(
          "Subsumptions",
          subs.zip(r1).map((x, y) => s"$x: $y")
        )

        log2.info(
          "Subsumptions",
          subs.zip(r2).map((x, y) => s"$x: $y")
        )

        r1 == r2
      else true

    // Perform second test: Comparison of results.

    val t2 = if compareResults then

      val c1 = s1.generateCandidates(q, log1)
      val c2 = s2.generateCandidates(q, log2)

      val r1 = s1.filter(c1, kb1, log1)
      val r2 = s2.filter(c2, kb2, log2)

      r1 == r2
    else true

    // Return results of first and second test.
    (t1, t2)

  // Test `n` trials to identify a difference.
  private def search(
      n: Int,
      qg: QueryGenerator,
      shapes: Int
  ): List[(SCCQ, Set[SimpleSHACLShape])] =
    def doSearch(trial: Int): List[(SCCQ, Set[SimpleSHACLShape])] =
      print(".")
      if trial <= 0 then Nil
      else
        // Sample query and shapes.
        val q = qg.draw
        val s = ShapeGenerator(q.pattern.vocabulary, shapes, true).draw

        // Initialize logs.
        val log1 = Log(debugging = true)
        val log2 = Log(debugging = true)
        s1.logInput(q, s, log1)
        s2.logInput(q, s, log2)

        val t = compare(q, s, log1, log2)

        if !t._1 || !t._2 then
          println()
          log1.print(true, true)
          log2.print(true, true)
          if stopAfterFirstResult then
            List((q, s))
          else 
            println(List.fill(80)("-").mkString(""))
            (q, s) :: doSearch(trial - 1)
        else doSearch(trial - 1)
    doSearch(n)

  // Run trials with this configuration.
  private def step(
      minNumberOfVariables: Int,
      minNumberOfConcepts: Int,
      minNumberOfProperties: Int,
      minNumberOfNominals: Int,
      minPatterns: Int,
      maxPatterns: Int,
      shapes: Int,
      trials: Int = stepTrials
  ): List[(SCCQ, Set[SimpleSHACLShape])] =
    print(
      List(
        minNumberOfVariables,
        minNumberOfConcepts,
        minNumberOfProperties,
        minNumberOfNominals,
        minPatterns,
        maxPatterns,
        shapes
      ).mkString(".")
    )
    val voc = VocabularyGenerator(
      minNumberOfVariables,
      minNumberOfVariables + 1,
      minNumberOfConcepts,
      minNumberOfConcepts + 1,
      minNumberOfProperties,
      minNumberOfProperties + 1,
      minNumberOfNominals,
      minNumberOfNominals + 1
    ).draw
    search(
      trials,
      QueryGenerator(voc, minPatterns, maxPatterns),
      shapes
    )

  // Generate set sets of random trials (with stepTrials each).
  private def random(set: Int): List[(SCCQ, Set[SimpleSHACLShape])] =
    if set <= 0 then Nil
    else
        val p1 = Random.between(1, 3)
        val p2 = Random.between(1, 3)
        val n = step(
          Random.between(1, 3),
          Random.between(1, 3),
          Random.between(1, 3),
          Random.between(0, 1), // TODO
          p1.min(p2),
          p1.max(p2) + 1,
          Random.between(1, 3)
        )
        n ++ random(set - 1)

  /** Run randomSets * stepTrials random trials. */
  def randomized(): Unit =
    for r <- random(randomSets)
    do println("\nExecuted " ++ r.toString ++ " trials without differences.")

  /** Widen search space in 10 steps * stepTrials, then finish with randomSets *
    * stepTrials random trials.
    */
  def structured(): Unit =
    // Widen search space with each step.

    // Only patterns x : A
    step(1, 2, 0, 0, 2, 4, 0)
    step(1, 2, 0, 0, 2, 4, 1)
    step(1, 2, 0, 0, 2, 4, 2)
    step(2, 2, 0, 0, 2, 4, 1)
    step(2, 2, 0, 0, 2, 4, 2)

    // Arbitrary patterns
    step(1, 2, 2, 0, 2, 4, 0)
    step(1, 2, 2, 0, 2, 4, 1)
    step(1, 2, 2, 0, 2, 4, 2)
    step(2, 2, 2, 0, 2, 4, 1)
    step(2, 2, 2, 0, 2, 4, 2)

    // Random trials
    random(randomSets)

  /** Compare one set of query and shapes. */
  def standalone(
      q: String,
      s: Set[String]
  ): Unit = 
    // Parse & setup logging.
    val qu = s1.parseQuery(q).toOption.get
    val sh = s1.parseShapes(s).toOption.get
    val log1 = Log(debugging = true)
    val log2 = Log(debugging = true)
    s1.logInput(qu, sh, log1)
    s2.logInput(qu, sh, log2)

    // Run single comparison.
    compare(qu, sh, log1, log2)

    // Format output.
    log1.print(true, true)
    log2.print(true, true)


object ConfigurationComparison:

    def full(q: String, s: Set[String]): Map[Set[SimpleSHACLShape], List[Configuration]] =
        (for 
            i <- 0 to 8192
            c = Configuration.fromBitset(i)
            s2s = Shapes2Shapes(c)
            qu = s2s.parseQuery(q).toOption.get
            sh = s2s.parseShapes(s).toOption.get
            l = Log(true, true)
            out = s2s.algorithm(qu, sh, l)
        yield (c, out)).groupBy(x => x._2).mapValues(_.map(_._1).toList).toMap