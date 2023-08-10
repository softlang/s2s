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

  // The number of samples to run.
  val trials = 5000

  // The seed for sample generation.
  val seed = "AAAI24"

  // The ActiveReasoner instance to use.
  // One of: Hermit, Jfact or Openllet.
  val reasoner = ActiveReasoner.Hermit

  // Timeout - maximum time allowed for reasoning per sample.
  val timeout = 60000

  // Retry samples this many times. This mitigates unlucky cases
  // for non-deterministic reasoner optimization strategies.
  val retry = 0

  // Experiment generator configurations (from the paper).

  // See ProblemGeneratorConfig Class for documentation (!)
  val small = ProblemGeneratorConfig(
    inputFile = None,
    minPatternSize = 1,
    maxPatternSize = 2,
    minTemplateSize = 1,
    maxTemplateSize = 2,
    freshVariable = 0.5f,
    variablesCount = 100,
    freshConcept = 0.8f,
    conceptsCount = 100,
    freshProperty = 0.8f,
    propertiesCount = 100,
    freshNominal = 0.9f,
    nominalsCount = 10,
    propertyConceptRatio = 0.3f,
    variableToNominalRatio = 0.9f,
    cyclicRedrawCount = 10,
    minNumberOfShapes = 1,
    maxNumberOfShapes = 2,
    propertyConceptTargetRatio = -1.0f,
    propertyConceptConstraintRatio = -1.0f,
    includeForallConstraints = true,
    seed = seed
  )

  val medium = small.copy(
    minPatternSize = 5,
    maxPatternSize = 7,
    minTemplateSize = 5,
    maxTemplateSize = 7,
    minNumberOfShapes = 5,
    maxNumberOfShapes = 7,
    propertyConceptTargetRatio = 0.3f,
    propertyConceptConstraintRatio = 0.3f
  )

  val large = small.copy(
    minPatternSize = 11,
    maxPatternSize = 13,
    minTemplateSize = 11,
    maxTemplateSize = 13,
    minNumberOfShapes = 11,
    maxNumberOfShapes = 13,
    propertyConceptTargetRatio = 0.3f,
    propertyConceptConstraintRatio = 0.3f
  )

  // See ./wikidata/Readme.md

  val wikidata_small = small.copy(
    inputFile = Some("wikidata/queries")
  )

  val wikidata_medium = medium.copy(
    inputFile = Some("wikidata/queries")
  )

  val wikidata_large = large.copy(
    inputFile = Some("wikidata/queries")
  )

  def run(): Unit =
    runConfig(small)
    runConfig(medium)
    runConfig(large)

    // runConfig(wikidata_small, trials = 60, drop = 2)
    // runConfig(wikidata_medium, trials = 60, drop = 2)
    // runConfig(wikidata_large, trials = 60, drop = 2)

  private def runConfig(
      pgc: ProblemGeneratorConfig,
      trials: Int = trials,
      drop: Int = 100
  ): Unit =
    Profiling(
      config = Configuration.default.copy(
        reasoner = reasoner,
        retry = retry,
        timeout = timeout
      ),
      noisy = true,
      logTime = false,
      logNoisy = false
    ).run(pgc, trials = trials, chunkCount = 10, dropFirstX = drop)
