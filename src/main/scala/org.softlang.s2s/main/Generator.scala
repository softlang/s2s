package org.softlang.s2s.main

import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.analysis.ProfileEntry
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

import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.Random

/** Observe Generator output, determining statistical information. */
class Statistics:

  case class Sample(
      query: GCORE,
      input: Set[SHACLShape],
      output: Set[SHACLShape],
      time: Long,
      candidates: Int
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
      outShapes: Int,
      shapeNesting: Double,
      shapeClauses: Double,
      shapeNegation: Int,
      time: Long,
      candidates: Int
  )

  extension [T, N: Numeric](l: Iterable[T])
    def minOver(f: T => N): N = l.map(f).min
    def maxOver(f: T => N): N = l.map(f).max
    def averageOver(f: T => N): Double =
      l.map(f).sum.toDouble / l.size.toDouble
    def medianOver(f: T => N): Double =
      val xs = l.map(f)
      xs match
        case Nil => -1
        case _ =>
          val sorted = xs.toList.sorted
          val n = sorted.length
          if n % 2 == 0 then
            val i = (n.toDouble / 2.0).toInt - 1
            val j = (n.toDouble / 2.0).toInt
            (sorted(i) + sorted(j)).toDouble / 2.0
          else
            val i = (n.toDouble / 2.0).toInt
            sorted(i).toDouble

    def statsOver(f: T => N): (N, N, Double, Double) =
      (minOver(f), maxOver(f), averageOver(f), medianOver(f))

    def pretty(name: String, f: T => N): String =
      val s = statsOver(f)
      val min = f"${s._1.toDouble}%.2f"
      val max = f"${s._2.toDouble}%.2f"
      val avg = f"${s._3.toDouble}%.2f"
      val med = f"${s._4.toDouble}%.2f"
      s"${name.take(15)}\t ${min}\t ${max}\t ${avg}\t ${med}\n"

    def prettyTime(name: String, f: T => N): String =
      val s = statsOver(f)
      val avg = f"${s._3.toDouble}%.2f"
      val med = f"${s._4.toDouble}%.2f"
      s"${name.take(15)}: ${avg} (${med})"

  /** Add a new sample to the statistics log. */
  def add(
      query: GCORE,
      input: Set[SHACLShape],
      output: Set[SHACLShape],
      time: Long,
      candidates: Int
  ): Unit =
    samples.addOne(Sample(query, input, output, time, candidates))

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
      outShapes = sample.output.size,
      shapeNesting = sample.input.map(_.depth).averageOver(identity),
      shapeClauses = sample.input.map(_.breadth).averageOver(identity),
      shapeNegation = sample.input.count(_.hasNegation),
      // Time
      time = sample.time,
      candidates = sample.candidates
    )

  private def stats: List[Stats] =
    samples.map(stat).toList

  def format(label: String, size: Int): String =
    "----------------------------------------------\n"
      + "SAMPLE: " + label + " (" + size.toString() + ") "
      + stats.prettyTime("Time", _.time) + "\n"
      + this.toString()

  override def toString(): String =
    val s = stats
    "----------------------------------------------\n"
      + "Type\t\tMin\tMax\tAverage\tMedian\n"
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
      + s.pretty("Cand. Shapes", _.candidates)
      + s.pretty("Shape Comp.", _.shapeNesting)
      + s.pretty("Shape Clauses", _.shapeClauses)
      + s.pretty("Shape Negation", _.shapeNegation)

/** Profiling for S2S from generated examples. */
object Generator
    extends Shapes2Shapes(
      Configuration.default.copy(
        reasoner = ActiveReasoner.Hermit,
        shapeHeuristic = ShapeHeuristic.NovaProGS(2, 1, opt = true) // TOGGLE
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
      keepNegation = 0.5f,
      propertyConceptTargetRatio = -1.0f,
      propertyConceptConstraintRatio = -1.0f,
      includeForallConstraints = true,
      sampleHeuristic = ShapeHeuristic.default
    ),
    seed = "TGDK2025"
  )

  // Basic configuration for generating normal looking queries.
  def baseConfig(seed: String) = GCOREProblemGeneratorConfig(
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
    vocExpansionFactor = 0.333f,
    patternRetention = 0.3f,
    labelsPerEntity = (0, 3),
    propsPerEntity = (0, 2),
    existToValueConstraints = 0.8f,
    targetSetClauses = (0, 3),
    targetRemoveClauses = (0, 2),
    targetWhenClauses = (1, 5),
    shapeConfig = ShapeGeneratorConfig.nova(
      min = 1,
      max = 4,
      heuristic = ShapeHeuristic.NovaProGS(2, 1, opt = true)
    ),
    seed = seed
  )

  // Small variant with simpler shapes and small queries.
  // This should produce the least number of empty result graphs
  // or non-existing targets.
  def smallConfig(seed: String) = baseConfig(seed).copy(
    minPatterns = 1,
    maxPatterns = 1,
    shapeConfig = ShapeGeneratorConfig.nova(
      min = 1,
      max = 1,
      heuristic = ShapeHeuristic.NovaProGS(1, 1, opt = true)
    )
  )

  // Configuration including con/disjunction in shapes.
  def wideConfig(seed: String) = baseConfig(seed).copy(
    shapeConfig = ShapeGeneratorConfig.nova(
      min = 1,
      max = 4,
      heuristic = ShapeHeuristic.NovaProGS(1, 2, opt = true)
    )
  )

  // Configuration including deeper shape constraint structures.
  def deepConfig(seed: String) = baseConfig(seed).copy(
    shapeConfig = ShapeGeneratorConfig.nova(
      min = 1,
      max = 4,
      heuristic = ShapeHeuristic.NovaProGS(2, 1, opt = true)
    )
  )

  // Configuration with (a single) very large input shapes.
  def largeConfig(seed: String) = baseConfig(seed).copy(
    shapeConfig = ShapeGeneratorConfig.nova(
      min = 3,
      max = 7,
      heuristic = ShapeHeuristic.NovaProGS(1, 1, opt = true)
    )
  )

  /** Samples per set, total of 8 * samples. */
  def run(samples_x16: Int, debug: Boolean = false): Unit =
    val samples = samples_x16

    // Run a few samples as warmup, so time measurement is accurate.
    generateGCORE(100, smallConfig("warmup"), "gen_warmup", debug, Statistics())

    val s1 = Statistics()
    generateGCORE(samples, smallConfig("TGDK_S_1"), "gen_small_1", debug, s1)
    generateGCORE(samples, smallConfig("TGDK_S_2"), "gen_small_2", debug, s1)
    generateGCORE(samples, smallConfig("TGDK_S_3"), "gen_small_3", debug, s1)
    generateGCORE(samples, smallConfig("TGDK_S_4"), "gen_small_4", debug, s1)
    println(s1.format("gen_small", samples * 4))

    val s2 = Statistics()
    generateGCORE(samples, deepConfig("TGDK_D_1"), "gen_deep_1", debug, s2)
    generateGCORE(samples, deepConfig("TGDK_D_2"), "gen_deep_2", debug, s2)
    generateGCORE(samples, deepConfig("TGDK_D_3"), "gen_deep_3", debug, s2)
    generateGCORE(samples, deepConfig("TGDK_D_4"), "gen_deep_4", debug, s2)
    println(s2.format("gen_deep", samples * 4))

    val s3 = Statistics()
    generateGCORE(samples, wideConfig("TGDK_W_1"), "gen_wide_1", debug, s3)
    generateGCORE(samples, wideConfig("TGDK_W_2"), "gen_wide_2", debug, s3)
    generateGCORE(samples, wideConfig("TGDK_W_3"), "gen_wide_3", debug, s3)
    generateGCORE(samples, wideConfig("TGDK_W_4"), "gen_wide_4", debug, s3)
    println(s3.format("gen_wide", samples * 4))

    val s4 = Statistics()
    generateGCORE(samples, largeConfig("TGDK_L_1"), "gen_large_1", debug, s4)
    generateGCORE(samples, largeConfig("TGDK_L_2"), "gen_large_2", debug, s4)
    generateGCORE(samples, largeConfig("TGDK_L_3"), "gen_large_3", debug, s4)
    generateGCORE(samples, largeConfig("TGDK_L_4"), "gen_large_4", debug, s4)
    println(s4.format("gen_large", samples * 4))

    // Generate a few failure samples, to validate the validator.
    val f = Statistics()
    generateGCORE(250, smallConfig("TGDK_F_1"), "gen_fail_1", debug, f, true)
    generateGCORE(250, deepConfig("TGDK_F_2"), "gen_fail_2", debug, f, true)
    generateGCORE(250, wideConfig("TGDK_F_3"), "gen_fail_3", debug, f, true)
    generateGCORE(250, largeConfig("TGDK_F_4"), "gen_fail_4", debug, f, true)
    println(f.format("gen_fail", 1000))

  private def generateGCORE(
      iterations: Int,
      config: GCOREProblemGeneratorConfig,
      label: String,
      debug: Boolean,
      stats: Statistics,
      fail: Boolean = false
  ): Unit =
    val scopes: Scopes = defaultScopes
    val ggen = ProblemGeneratorPG(config)(scopes)
    val vgen = ValidationDataGenerator(shar.state)

    var it = 0

    while (it < iterations) do
      if debug then println(s"\nSAMPLE::${it}")

      val (query, shapes) = ggen.sample()
      val input = AlgorithmInput.fromSetOfShapesGCORE(query, shapes, scopes)

      if debug then
        println("\n\n::: Sampled Input Query :::\n")
        println(input.formatQuery(shar.state))
        println("\n\n::: Sampled Input Shapes :::\n")
        println(input.formatShapes.toOption.getOrElse(""))

      val start = System.nanoTime()
      val (tryoutput, log) = constructShapes(input)
      val end = System.nanoTime()
      val durationNanos = end - start
      val time = durationNanos / 1_000_000

      if debug then
        println("\n\n::: Generated Outputs :::\n")
        println(log)

      tryoutput match
        case Left(err) =>
          println("Generator error: " + err.toString)
        case Right(output) =>
          if !output.isEmpty then
            // If we deliberately want failing samples,
            // just generate some random shapes instead.
            val out =
              if fail then
                val candidates = log.profile.flatMap { p =>
                  p match
                    case ProfileEntry.Candidates(c, _) => Some(c)
                    case _                             => None
                }.flatten
                Random().shuffle(candidates).take(7).toSet
              else output
            vgen.generate(
              input,
              out,
              "gen",
              log,
              gen = true,
              genSubDir = label
            )
            val candidates = log.profile.flatMap { p =>
              p match
                case ProfileEntry.Candidates(c, _) => Some(c.size)
                case _                             => None
            }.sum
            stats.add(query, shapes, out, time, candidates)
            it += 1
