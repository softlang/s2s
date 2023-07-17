package org.softlang.s2s.main

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Configuration
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.Util
import org.softlang.s2s.core.ShapeHeuristic

import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.infer.Shapes2Shapes

/** Stand-alone problem generator. */
object Dev:

  private def concepts(n: Int): Set[NamedConcept] =
    (for i <- Range(0, n) yield NamedConcept(Util.forceIriUnsave(s"C$i"))).toSet

  private def properties(n: Int): Set[NamedRole] =
    (for i <- Range(0, n) yield NamedRole(Util.forceIriUnsave(s"p$i"))).toSet

  private def genCount(cs: Int, ps: Int): Int =
    val gen = ShapeGenerator(
      Vocabulary(
        variables = Set(),
        concepts = concepts(cs),
        properties = properties(ps),
        nominals = Set()
      ),
      ShapeHeuristic.default
    )
    gen.generate.size

  def run(): Unit =
    for
      c <- Range(0, 3)
      p <- Range(0, 3)
    do println(s"$c $p ${genCount(c, p)}")

    // val s2s = Shapes2Shapes(Configuration.default)
    // val config =
    //  ProblemGeneratorConfig(
    //    // Min/max count of atomic patterns in Pattern.
    //    minPatternSize = (3, 5),
    //    maxPatternSize = (5, 7),
    //    // Min/max count of atomic patterns in Template.
    //    minTemplateSize = 3,
    //    maxTemplateSize = 5,
    //    // Probability of generating a fresh variable (0.0 to 1.0).
    //    freshVariable = 0.8f,
    //    // Maximum number of variables.
    //    variablesCount = 2,
    //    // Probability of generating a fresh concept (0.0 to 1.0).
    //    freshConcept = 1.0f,
    //    // Total number of concepts allowed.
    //    conceptsCount = 10,
    //    // Probability of generating a fresh property (0.0 to 1.0).
    //    freshProperty = 1.0f,
    //    // Total number of properties allowed.
    //    propertiesCount = 10,
    //    // Probability of generating a fresh nominal (0.0 to 1.0).
    //    freshNominal = 0.0f,
    //    // Total number of nominals allowed.
    //    nominalsCount = 0,
    //    // Ratio of property patterns to concept patterns (0.0 to 1.0).
    //    propertyConceptRatio = 0.5f,
    //    // Ratio of variables to nominals in patterns (0.0 to 1.0).
    //    variableToNominalRatio = 1.0f,
    //    // Avoid self-circles by redrawing N times.
    //    cyclicRedrawCount = 10,
    //    // Min/max number of input shapes.
    //    minNumberOfShapes = 8,
    //    maxNumberOfShapes = 10,
    //    // Ratio of property-based vs. Concept targets.
    //    propertyConceptTargetRatio = -1.0f,
    //    // Ratio of property (exists, forall) vs. Concept constraints.
    //    propertyConceptConstraintRatio = -1.0f,
    //    // Ratio of existential vs. universal quantification in constraints.
    //    includeForallConstraints = false,
    //    // Random seed. Use "" for random seed.
    //    seed = ""
    //  )

    // println(config)

    // val gen = ProblemGenerator(config)(s2s.scopes)

    // val qs = gen.sample()
    // val q = qs._1
    // val s = qs._2

    // println(q.show(s2s.shar.state))
    // s.foreach(si => println(si.show(s2s.shar.state)))
