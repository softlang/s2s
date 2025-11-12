package org.softlang.s2s.main

import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.infer.AlgorithmInput

/** Profiling for S2S from generated examples. */
object Generator extends Shapes2Shapes:

  import scala.language.implicitConversions

  // See ProblemGeneratorConfig Class for documentation (!)
  val config = SCCQProblemGeneratorConfig(
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
    shapeConfig = ShapeGeneratorConfig(
      minNumberOfShapes = 1,
      maxNumberOfShapes = 2,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = true
    ),
    seed = "TGDK2025"
  )

  def run(iterations: Int, debug: Boolean = true): Unit =
    val scopes: Scopes = defaultScopes
    val gen = ProblemGeneratorRDF(config)(scopes)
    val vgen = ValidationDataGenerator(shar.state)

    for i <- 0 until iterations do
      val (query, shapes) = gen.sample()

      // TODO: Problem generator for ProGS/G-CORE shapes and queries.

      val input = AlgorithmInput.fromSetOfShapes(query, shapes, scopes)

      if debug then
        println("\n\n::: Sampled Input Query :::\n")
        println(input.formatQuery(shar.state))
        println("\n\n::: Sampled Input Shapes :::\n")
        println(input.formatShapes.toOption.getOrElse(""))

      val (tryoutput, log) = constructShapes(input)

      if debug then
        println("\n\n::: Generated Outputs :::\n")
        println(log)

      // TODO: Missing step -- use some heuristic or approach
      // to generate actual shapes from the result axioms.
      //
      // SimpleSHACL: Trivial, simple coding challenge?
      // ProGS: Must define some subset and enumerate, I guess?
      //  - Implement in core, based on SimpleSHACL approach
      //  - Then extend features with some features, take heuristic as argument
      //  - Implement some heuristics, random sample for validation

      tryoutput match
        case Left(err) => println("Generator error: " + err.toString)
        case Right(output) =>
          vgen.generate(input, output, "gen", log, gen = true)
