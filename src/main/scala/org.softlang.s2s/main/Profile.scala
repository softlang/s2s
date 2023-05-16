package org.softlang.s2s.main

import org.softlang.s2s.analysis._
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange

/** Profiling for S2S from generated examples. */
object Profile:

  def run(): Unit =

    val trials = 1000
    val seed = "sub"

    // helper(
    //   retry = 0,
    //   timeout = 60000,
    //   reasoner = ActiveReasoner.Hermit,
    //   trials = trials,
    //   pgc = ProblemGeneratorConfig(
    //     minPatternSize = 2,
    //     maxPatternSize = 3,
    //     minTemplateSize = 2,
    //     maxTemplateSize = 3,
    //     freshVariable = 0.8f,
    //     variablesCount = (1, 4),
    //     freshConcept = 0.9f,
    //     conceptsCount = (1, 10),
    //     freshProperty = 0.9f,
    //     propertiesCount = (1, 10),
    //     freshNominal = 0.9f,
    //     nominalsCount = (0, 3),
    //     propertyConceptRatio = 0.3f,
    //     variableToNominalRatio = 0.9f,
    //     cyclicRedrawCount = 10,
    //     minNumberOfShapes = 1,
    //     maxNumberOfShapes = 4,
    //     propertyConceptTargetRatio = -1.0f,
    //     propertyConceptConstraintRatio = -1.0f,
    //     includeForallConstraints = true,
    //     seed = seed
    //   )
    // )

    helper(
      retry = 0,
      timeout = 100, // skip reasoning
      reasoner = ActiveReasoner.Hermit,
      trials = trials,
      pgc = ProblemGeneratorConfig(
        minPatternSize = 50,
        maxPatternSize = 51,
        minTemplateSize = 50,
        maxTemplateSize = 51,
        freshVariable = 0.7f,
        variablesCount = (10, 100),
        freshConcept = 0.7f,
        conceptsCount = (100, 200),
        freshProperty = 0.7f,
        propertiesCount = (100, 200),
        freshNominal = 0.7f,
        nominalsCount = (0, 10),
        propertyConceptRatio = 0.3f,
        variableToNominalRatio = 0.9f,
        cyclicRedrawCount = 10,
        minNumberOfShapes = 100,
        maxNumberOfShapes = 101,
        propertyConceptTargetRatio = -1.0f,
        propertyConceptConstraintRatio = -1.0f,
        includeForallConstraints = true,
        seed = seed
      )
    )

  private def helper(
      // Retry samples this many times. This mitigates unlucky cases
      // for non-deterministic reasoner optimization strategies.
      retry: Int,
      // Timeout - maximum time allowed for reasoning.
      timeout: Int,
      // The ActiveReasoner instance to use.
      // One of: Hermit, Jfact or Openllet.
      reasoner: ActiveReasoner,
      // The number of samples to run.
      trials: Int,
      // The generation of the problem generator to use.
      pgc: ProblemGeneratorConfig
  ): Unit =
    Profiling(
      config = Configuration.default.copy(
        activeReasoner = reasoner,
        retry = retry,
        timeout = timeout
      ),
      noisy = true,
      logTime = false,
      logNoisy = false
    ).run(pgc, trials = trials, chunkCount = 10)
