package org.softlang.s2s.analysis

import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.dl.Subsumption
import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.vocabulary

class ConfigurationComparison(
    c1: Configuration,
    c2: Configuration,
    trials: Int = 1000,
    stopAfterFirstResult: Boolean = true,
    title1: String = "",
    title2: String = ""
):

  import scala.language.implicitConversions

  // Test S2S Instances.
  private val s1 = Shapes2Shapes(c1)
  private val s2 = Shapes2Shapes(c2)

  /** Compare results for one set of query and shapes. */
  private def compare(
      q: SCCQ,
      s: Set[SHACLShape],
      log1: Log,
      log2: Log
  ): Boolean = s1.algorithm(q, s, log1) == s2.algorithm(q, s, log2)

  /** Test with generator setup for trials many runs. */
  private def search(
      qg: ProblemGenerator,
      verbose: Boolean = true
  ): List[(SCCQ, Set[SHACLShape])] =
    def doSearch(trial: Int): List[(SCCQ, Set[SHACLShape])] =
      if trial <= 0 then Nil
      else

        // Sample query and shapes.
        val qs = qg.sample()
        val q = qs._1
        val s = qs._2

        // Progress
        if verbose then
          println("\nProblem:")
          println(q.show(s1.shar.state))
          s.map(_.show(s1.shar.state)).foreach(println)
        else print(".")

        // Initialize logs.
        val log1 = Log(debugging = true)(s1.scopes)
        val log2 = Log(debugging = true)(s2.scopes)

        // If results are different
        if !compare(q, s.toList.toSet, log1, log2) then
          // print the logs.
          println()
          log1.print(true, true)
          log2.print(true, true)
          // Return this result, or continue (if allowed).
          if stopAfterFirstResult then List((q, s.toList.toSet))
          else
            println(List.fill(80)("-").mkString(""))
            (q, s.toList.toSet) :: doSearch(trial - 1)
        // If there was no difference, continue with next trial.
        else doSearch(trial - 1)
    doSearch(trials)

  /** One step, with a generator config an number of trials. */
  private def step(config: ProblemGeneratorConfig): Unit =
    print(config)
    search(ProblemGenerator(config)(s1.scopes))
    println("done.")

  /** Compare for specific input (String encoded). */
  def input(q: String, sin: Set[String]): S2STry[Boolean] =
    for
      qp <- s1.parseSCCQQuery(q)
      sp <- s1.parseSHACLShapes(sin)
    yield input(qp, sp.toList.toSet)

  /** Compare for specific input. */
  def input(q: SCCQ, sin: Set[SHACLShape]): Boolean =

    // Initialize logs.
    val log1 = Log(debugging = true)(s1.scopes)
    val log2 = Log(debugging = true)(s2.scopes)

    val r = compare(q, sin, log1, log2)

    println("Configuration (1) " ++ title1)
    log1.print(true, true)

    println("\nConfiguration (2) " ++ title2)
    log2.print(true, true)

    r

  def structured(): Unit =
    // Widen search with various configurations.

    // Only concepts.
    // step(
    //  ProblemGeneratorConfig(
    //    minPatternSize = (3, 5),
    //    maxPatternSize = (5, 7),
    //    minTemplateSize = 3,
    //    maxTemplateSize = 5,
    //    freshVariable = 0.8f,
    //    variablesCount = 1,
    //    freshConcept = 1.0f,
    //    conceptsCount = 10,
    //    freshProperty = 1.0f,
    //    propertiesCount = 10,
    //    freshNominal = 0.0f,
    //    nominalsCount = -1,
    //    propertyConceptRatio = 0.0f,
    //    variableToNominalRatio = 1.0f,
    //    minNumberOfShapes = 1,
    //    maxNumberOfShapes = 3,
    //    propertyConceptTargetRatio = -1.0f,
    //    propertyConceptConstraintRatio = -1.0f,
    //    includeForallConstraints = false
    //  )
    // )

    val rc = ProblemGeneratorConfig(
      inputFile = None,
      minPatternSize = 2,
      maxPatternSize = 6,
      minTemplateSize = 2,
      maxTemplateSize = 6,
      freshVariable = (0.0f, 1.0f),
      variablesCount = (1, 10),
      freshConcept = (0.0f, 1.0f),
      conceptsCount = (1, 10),
      freshProperty = (0.0f, 1.0f),
      propertiesCount = (1, 10),
      freshNominal = 0.0f,
      nominalsCount = (0, 10),
      propertyConceptRatio = (0.0f, 1.0f),
      variableToNominalRatio = 0.8f,
      cyclicRedrawCount = 10,
      minNumberOfShapes = 0,
      maxNumberOfShapes = 2,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = true
    )

    step(rc)
    // step(rc)
    // step(rc)
    // step(rc)
    // step(rc)
