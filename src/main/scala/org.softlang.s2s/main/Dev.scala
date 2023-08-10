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
