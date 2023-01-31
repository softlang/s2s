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
    // experiment1()
    test()
    ()

  private def test(): Unit =
    val config = Configuration.default.copy(
      activeReasoner = ActiveReasoner.Hermit,
      retry = 0,
      timeout = 60000
    )

    val profiling = Profiling(
      config,
      noisy = true,
      logTime = false,
      logNoisy = false
    )

    val pgc = ProblemGeneratorConfig(
      minPatternSize = 2,
      maxPatternSize = 2,
      minTemplateSize = 5,
      maxTemplateSize = 5,
      freshVariable = 0.8f,
      variablesCount = (1, 5),
      freshConcept = 0.8f,
      conceptsCount = 10,
      freshProperty = 0.8f,
      propertiesCount = 10,
      freshNominal = 0.0f,
      nominalsCount = 0,
      propertyConceptRatio = 0.6f,
      variableToNominalRatio = 1.0f,
      cyclicRedrawCount = 10,
      minNumberOfShapes = 0,
      maxNumberOfShapes = 0,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = false,
      seed = "minimal"
    )

    profiling.run(pgc, trials = 1000, chunkCount = 10, dropFirstX = 0)

  private def experiment1(): Unit =
    experiment1Run(5, 10000, ActiveReasoner.Hermit)
    experiment1Run(5, 10000, ActiveReasoner.Openllet)
    experiment1Run(2, 30000, ActiveReasoner.Hermit)
    experiment1Run(2, 30000, ActiveReasoner.Hermit)
    experiment1Run(0, 60000, ActiveReasoner.Hermit)
    experiment1Run(2, 30000, ActiveReasoner.Openllet)
    experiment1Run(0, 60000, ActiveReasoner.Openllet)

  private def experiment1Run(
      retry: Int,
      timeout: Int,
      reasoner: ActiveReasoner
  ): Unit =

    val config = Configuration.default.copy(
      activeReasoner = reasoner,
      retry = retry,
      timeout = timeout
    )

    val profiling = Profiling(
      config,
      noisy = false,
      logTime = false,
      logNoisy = false
    )

    val pgc = ProblemGeneratorConfig(
      minPatternSize = 1,
      maxPatternSize = 8,
      minTemplateSize = 1,
      maxTemplateSize = 8,
      freshVariable = 0.8f,
      variablesCount = (1, 5),
      freshConcept = 0.5f,
      conceptsCount = (2, 4),
      freshProperty = 0.8f,
      propertiesCount = (2, 4),
      freshNominal = 0.0f,
      nominalsCount = 0,
      propertyConceptRatio = 0.3f,
      variableToNominalRatio = 1.0f,
      cyclicRedrawCount = 10,
      minNumberOfShapes = 0,
      maxNumberOfShapes = 6,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = false,
      seed = "letsgo"
    )

    profiling.run(pgc, trials = 1000, chunkCount = 10)
