package org.softlang.s2s.main

import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.infer.AlgorithmInput
import de.pseifer.shar.core.Prefix
import de.pseifer.shar.core.Iri

/** Observe Generator output, determining statistical information. */
class Statistics:
  var x = 42

/** Profiling for S2S from generated examples. */
object Generator
    extends Shapes2Shapes(
      Configuration.default.copy(
        reasoner = ActiveReasoner.Hermit,
        shapeHeuristic = ShapeHeuristic.NovaProGS(1, 1, opt = true) // TOGGLE
      )
    ):

  import scala.language.implicitConversions

  // Add GCORE prefixes.
  for
    pln <- Prefix.fromString("ln:")
    ln <- Iri.fromString("<https://github.com/softlang/s2s/nlabel/>")
    ple <- Prefix.fromString("le:")
    le <- Iri.fromString("<https://github.com/softlang/s2s/elabel/>")
    pkn <- Prefix.fromString("kn:")
    kn <- Iri.fromString("<https://github.com/softlang/s2s/nkey/>")
    pke <- Prefix.fromString("ke:")
    ke <- Iri.fromString("<https://github.com/softlang/s2s/ekey/>")
    pm <- Prefix.fromString("m:")
    m <- Iri.fromString("<https://github.com/softlang/s2s/m/>")
  do {
    shar.state.prefixes.add(pln, ln)
    shar.state.prefixes.add(ple, le)
    shar.state.prefixes.add(pkn, kn)
    shar.state.prefixes.add(pke, ke)
    shar.state.prefixes.add(pke, ke)
    shar.state.prefixes.add(pm, m)
  }

  // See ProblemGeneratorConfig Class for documentation (!)
  val sconfig = SCCQProblemGeneratorConfig(
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

  // See ProblemGeneratorConfig Class for documentation (!)
  val gconfig = GCOREProblemGeneratorConfig(
    freshValue = 1.0f,
    valuesCount = 100,
    freshVariable = 0.5f,
    variablesCount = 5,
    freshKey = 0.7f,
    keysCount = 5,
    freshLabel = 0.7f,
    labelsCount = 5,
    edgeNodeRatio = 0.9f,
    minPatterns = 1,
    maxPatterns = 2,
    freshEntities = 0.2f,
    loopRedraw = 0.9f,
    patternRetention = 0.3f,
    labelsPerEntity = (0, 3),
    propsPerEntity = (0, 2),
    existToValueConstraints = 0.8f,
    targetSetClauses = (0, 2),
    targetRemoveClauses = (0, 2),
    shapeConfig = ShapeGeneratorConfig(
      minNumberOfShapes = 1,
      maxNumberOfShapes = 2,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = false
    )
    // seed = "TGDK2025"
  )

  def run(iterations: Int, debug: Boolean): Unit =
    val scopes: Scopes = defaultScopes
    val sgen = ProblemGeneratorRDF(sconfig)(scopes)
    val ggen = ProblemGeneratorPG(gconfig)(scopes)

    val vgen = ValidationDataGenerator(shar.state)

    var it = 0

    while (it < iterations) do
      println(s"\nSAMPLE::${it}")

      val (query, shapes) = ggen.sample()
      val input = AlgorithmInput.fromSetOfShapesGCORE(query, shapes, scopes)

      if debug then
        println("\n\n::: Sampled Input Query :::\n")
        println(input.formatQuery(shar.state))
        println("\n\n::: Sampled Input Shapes :::\n")
        println(input.formatShapes.toOption.getOrElse(""))

      // TODO TEMP
      // println("\n\n::: Sampled GCORE Query :::\n")
      // println(query.show(shar.state))
      // println("\n\n::: Mapped SPARQL Query :::\n")
      // println(input.formatQuery(shar.state))
      // TODO TEMP

      val (tryoutput, log) = constructShapes(input)

      if debug then
        println("\n\n::: Generated Outputs :::\n")
        println(log)

      // TODO: Missing step -- use some heuristic or approach
      // to generate actual shapes from the result axioms.
      //
      // NOTE: This is already implemented in Algorithm.shapes;
      // all that is missing is fine-tuning the heuristics I think!
      //
      // SimpleSHACL: Trivial, simple coding challenge?
      // ProGS: Must define some subset and enumerate, I guess?
      //  - Implement in core, based on SimpleSHACL approach
      //  - Then extend features with some features, take heuristic as argument
      //  - Implement some heuristics, random sample for validation

      //  TODO: Really, if the set of result shapes is empty for a sample,
      //  We should not store it, but try again, right? There is absolutely
      //  no reason to run Python validation on such samples.

      tryoutput match
        case Left(err) =>
          println("Generator error: " + err.toString)
        case Right(output) =>
          if !output.isEmpty then
            vgen.generate(input, output, "gen", log, gen = true)
            it += 1

    // for i <- 0 until iterations do
    //   val (query, shapes) = sgen.sample()

    //   val input = AlgorithmInput.fromSetOfShapesSCCQ(query, shapes, scopes)

    //   if debug then
    //     println("\n\n::: Sampled Input Query :::\n")
    //     println(input.formatQuery(shar.state))
    //     println("\n\n::: Sampled Input Shapes :::\n")
    //     println(input.formatShapes.toOption.getOrElse(""))

    //   val (tryoutput, log) = constructShapes(input)

    //   if debug then
    //     println("\n\n::: Generated Outputs :::\n")
    //     println(log)

    //   tryoutput match
    //     case Left(err) => println("Generator error: " + err.toString)
    //     case Right(output) =>
    //       vgen.generate(input, output, "gen", log, gen = true)
