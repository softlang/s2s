package org.softlang.s2s.generate

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core._
import org.softlang.s2s.query._

import scala.util.Random

/** Generator for queries over a vocabulary. */
class OldQueryGenerator(
    // Defines variables, concepts, properties and nominals (optional).
    voc: Vocabulary,
    // Minimum number of patterns.
    minPatterns: Int,
    // Maximum number of patterns.
    maxPatterns: Int
):

  /** Generate one random atomic pattern. */
  private def generateAtomicPattern(voc: Vocabulary): AtomicPattern =
    if voc.properties.isEmpty then
      val f = Random.shuffle(pGnNnP).head
      f(
        Random.shuffle(voc.variables.toList).head,
        Random.shuffle(voc.concepts.toList).head
      )
    else if voc.nominals.isEmpty then
      val f = Random.shuffle(pGnN).head
      f(
        Random.shuffle(voc.variables.toList).head,
        Random.shuffle(voc.variables.toList).head,
        Random.shuffle(voc.properties.toList).head,
        Random.shuffle(voc.concepts.toList).head
      )
    else
      val f = Random.shuffle(pG).head
      f(
        Random.shuffle(voc.variables.toList).head,
        Random.shuffle(voc.variables.toList).head,
        Random.shuffle(voc.properties.toList).head,
        Random.shuffle(voc.concepts.toList).head,
        Random.shuffle(voc.nominals.toList).head,
        Random.shuffle(voc.nominals.toList).head
      )

  /** Generate random query patterns. */
  private def generatePatterns(voc: Vocabulary): AtomicPatterns =
    List
      .fill(maxPatterns)(generateAtomicPattern(voc))
      .take(
        Random.between(minPatterns, maxPatterns)
      )

  /** Drawn a random SCCQ. */
  def draw: SCCQ =
    val p = generatePatterns(voc)
    val h = generatePatterns(
      Vocabulary(
        p.variables,
        voc.concepts,
        voc.properties,
        voc.nominals
      ).triviallyRenamed
    )
    SCCQ(h, p)

  /** Generate all possible shapes over vocabulary. */
  private val queries = Set()
  Random.shuffle(queries.toList).take(1).toSet

  /** Possible pattern generators. */
  private def pG
      : List[(Var, Var, NamedRole, NamedConcept, Iri, Iri) => AtomicPattern] =
    import AtomicPattern._
    List(
      (v1, v2, p, c, i1, i2) => LAC(i1, c.c),
      (v1, v2, p, c, i1, i2) => VAC(v1, c.c),
      (v1, v2, p, c, i1, i2) => LPL(i1, p.r, i2),
      (v1, v2, p, c, i1, i2) => VPL(v1, p.r, i1),
      (v1, v2, p, c, i1, i2) => LPV(i1, p.r, v1),
      (v1, v2, p, c, i1, i2) => VPV(v1, p.r, v2)
    )

  /** Possible pattern generators, no nominals. */
  private def pGnN: List[(Var, Var, NamedRole, NamedConcept) => AtomicPattern] =
    import AtomicPattern._
    List(
      (v1, v2, p, c) => VAC(v1, c.c),
      (v1, v2, p, c) => VPV(v1, p.r, v2)
    )

  /** Possible pattern generators, no nominals. */
  private def pGnNnP: List[(Var, NamedConcept) => AtomicPattern] =
    import AtomicPattern._
    List((v1, c) => VAC(v1, c.c))
