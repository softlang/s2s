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

import scala.collection.mutable.ListBuffer as MList
import org.softlang.s2s.query.GCORE

/** Observe Generator output, determining statistical information. */
class Statistics:

  case class Sample(
      query: GCORE,
      input: Set[SHACLShape],
      output: Set[SHACLShape]
  )

  private val samples: MList[Sample] = MList.empty

  case class Stats(
      nodeVariables: Int,
      edgeVariables: Int,
      atomPattern: Int,
      atomTemplate: Int,
      whenClause: Int,
      setClause: Int,
      removeClause: Int,
      distinctLabels: Int,
      distinctProperties: Int,
      inShapes: Int,
      outShapes: Int
  )

  extension (l: List[Stats])
    def minOver(f: Stats => Int): Int = l.map(f).min
    def maxOver(f: Stats => Int): Int = l.map(f).max
    def averageOver(f: Stats => Int): Double =
      l.map(f).sum.toDouble / l.size.toDouble
    def medianOver(f: Stats => Int): Double =
      val xs = l.map(f)
      xs match
        case Nil => -1
        case _ =>
          val sorted = xs.sorted
          val n = sorted.length
          if n % 2 == 0 then (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0
          else sorted(n / 2)

    def statsOver(f: Stats => Int): (Int, Int, Double, Double) =
      (minOver(f), maxOver(f), averageOver(f), medianOver(f))

    def pretty(name: String, f: Stats => Int): String =
      val s = statsOver(f)
      s"${name.take(15)}\t ${s._1}\t ${s._2}\t ${s._3}\t ${s._4}\n"

  /** Add a new sample to the statistics log. */
  def add(
      query: GCORE,
      input: Set[SHACLShape],
      output: Set[SHACLShape]
  ): Unit =
    samples.addOne(Sample(query, input, output))

  private def stat(sample: Sample): Stats =
    Stats(
      // Variables (Distinct)
      nodeVariables = sample.query.nodeVariables.size,
      edgeVariables = sample.query.edgeVariables.size,
      // Query Shape
      atomPattern = sample.query.pattern.fullGraphPattern.size,
      atomTemplate = sample.query.template.fullGraphPattern.size,
      whenClause = sample.query.pattern.when.size,
      setClause = sample.query.template.set.size,
      removeClause = sample.query.template.remove.size,
      // Vocabulary
      distinctLabels = sample.query.labels.size,
      distinctProperties = sample.query.keys.size,
      // Shapes
      inShapes = sample.input.size,
      outShapes = sample.output.size
    )

  private def stats: List[Stats] =
    samples.map(stat).toList

  override def toString(): String =
    val s = stats
    "Type\t\tMin\tMax\tAverage\tMedian\n"
      + "----------------------------------------------\n"
      + s.pretty("Node Variables", _.nodeVariables)
      + s.pretty("Edge Variables", _.edgeVariables)
      + s.pretty("Atoms MATCH", _.atomPattern)
      + s.pretty("Atoms CONSTRUCT", _.atomTemplate)
      + s.pretty("WHEN Clauses", _.whenClause)
      + s.pretty("SET Clauses", _.setClause)
      + s.pretty("REMOVE Clauses", _.removeClause)
      + s.pretty("Distinct Labels", _.distinctLabels)
      + s.pretty("Distinct Keys", _.distinctProperties)
      + s.pretty("Input Shapes", _.inShapes)
      + s.pretty("Output Shapes", _.outShapes)

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
      includeForallConstraints = true,
      sampleHeuristic = ShapeHeuristic.default
    ),
    seed = "TGDK2025"
  )

  // See ProblemGeneratorConfig Class for documentation (!)
  val gconfig = GCOREProblemGeneratorConfig(
    freshValue = 1.0f,
    valuesCount = 100,
    freshVariable = 0.5f,
    variablesCount = 5,
    freshKey = 0.75f,
    keysCount = 5,
    freshLabel = 0.75f,
    labelsCount = 5,
    edgeNodeRatio = 0.9f,
    minPatterns = 1,
    maxPatterns = 3,
    freshEntities = 0.2f,
    loopRedraw = 0.9f,
    patternRetention = 0.3f,
    labelsPerEntity = (0, 3),
    propsPerEntity = (0, 2),
    existToValueConstraints = 0.8f,
    targetSetClauses = (0, 3),
    targetRemoveClauses = (0, 2),
    targetWhenClauses = (1, 5),
    shapeConfig = ShapeGeneratorConfig(
      minNumberOfShapes = 1,
      maxNumberOfShapes = 4,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = false,
      sampleHeuristic = ShapeHeuristic.NovaProGS(2, 1, opt = true)
    ),
    seed = "TGDK2025"
  )

  def run(iterations: Int, debug: Boolean): Unit =
    val scopes: Scopes = defaultScopes
    val sgen = ProblemGeneratorRDF(sconfig)(scopes)
    val ggen = ProblemGeneratorPG(gconfig)(scopes)

    val vgen = ValidationDataGenerator(shar.state)

    val stats = Statistics()

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

      val (tryoutput, log) = constructShapes(input)

      if debug then
        println("\n\n::: Generated Outputs :::\n")
        println(log)

      tryoutput match
        case Left(err) =>
          println("Generator error: " + err.toString)
        case Right(output) =>
          if !output.isEmpty then
            vgen.generate(input, output, "gen", log, gen = true)
            stats.add(query, shapes, output)
            it += 1

    println(stats)

    // SPARQL Variant (TODO)
    //
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
