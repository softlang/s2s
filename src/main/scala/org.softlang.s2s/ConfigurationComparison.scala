package org.softlang.s2s

import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.dl.Subsumption
import org.softlang.s2s.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.vocabulary

import scala.collection.BitSet
import scala.util.Random

class ConfigurationComparison(
    c1: Configuration,
    c2: Configuration,
    trials: Int = 1000,
    stopAfterFirstResult: Boolean = true
):

  // Test S2S Instances.
  private val s1 = Shapes2Shapes(c1)
  private val s2 = Shapes2Shapes(c2)

  /** Compare results for one set of query and shapes. */
  private def compare(
      q: SCCQ,
      s: Set[SimpleSHACLShape],
      log1: Log,
      log2: Log
  ): Boolean = s1.algorithm(q, s, log1) == s2.algorithm(q, s, log2)

  /** Test with generator setup for trials many runs. */
  private def search(
      qg: ProblemGenerator
  ): List[(SCCQ, Set[SimpleSHACLShape])] =
    def doSearch(trial: Int): List[(SCCQ, Set[SimpleSHACLShape])] =

      if trial <= 0 then Nil
      else

        // Sample query and shapes.
        val qs = qg.sample()
        val q = qs._1
        val s = qs._2

        // Progress
        println(q.show(s1.shar.state))
        s.map(_.show(s1.shar.state)).foreach(println)

        // Initialize logs.
        val log1 = Log(debugging = true, "T")
        val log2 = Log(debugging = true, "T")

        // If results are different
        if !compare(q, s, log1, log2) then
          // print the logs.
          println()
          log1.print(true, true)
          log2.print(true, true)
          // Return this result, or continue (if allowed).
          if stopAfterFirstResult then List((q, s))
          else
            println(List.fill(80)("-").mkString(""))
            (q, s) :: doSearch(trial - 1)
        // If there was no difference, continue with next trial.
        else doSearch(trial - 1)
    doSearch(trials)

  /** One step, with a generator config an number of trials. */
  private def step(config: ProblemGeneratorConfig): Unit =
    print(config)
    search(ProblemGenerator(config)(s1.scopes))
    println("done.")

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
      minPatternSize = (1, 9),
      maxPatternSize = (9, 10),
      minTemplateSize = (1, 9),
      maxTemplateSize = (9, 10),
      freshVariable = (0.0f, 1.0f),
      variablesCount = (1, 10),
      freshConcept = (0.0f, 1.0f),
      conceptsCount = (1, 10),
      freshProperty = (0.0f, 1.0f),
      propertiesCount = (1, 10),
      freshNominal = 0.0f,
      nominalsCount = 0,
      propertyConceptRatio = (0.0f, 1.0f),
      variableToNominalRatio = 1.0f,
      cyclicRedrawCount = 10,
      minNumberOfShapes = (1, 9),
      maxNumberOfShapes = (9, 10),
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = true
    )

    step(rc)
    // step(rc)
    // step(rc)
    // step(rc)
    // step(rc)
