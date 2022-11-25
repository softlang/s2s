package org.softlang.s2s.generate

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Vocabulary

import scala.util.Random

/** Generate variables, concepts, properties and nominals (between 0 and 6). */
class VocabularyGenerator(
    minNumberOfVariables: Int = 6,
    maxNumberOfVariables: Int = 7,
    minNumberOfConcepts: Int = 6,
    maxNumberOfConcepts: Int = 7,
    minNumberOfProperties: Int = 6,
    maxNumberOfProperties: Int = 7,
    minNumberOfNominals: Int = 6,
    maxNumberOfNominals: Int = 7
):

  private val pre = "https://github.com/softlang/s2s/"

  private def mkIri(s: String): Iri = Iri
    .makeFromRawIri(
      pre ++ s
    )
    .toOption
    .get

  private val v: List[Var] =
    List("X", "Y", "Z", "U", "V", "W").map(Var.apply)

  private val c: List[NamedConcept] =
    List("A", "B", "C", "D", "E", "F").map(ci => NamedConcept(mkIri(ci)))

  private val p: List[NamedRole] =
    List("p", "q", "r", "s", "t", "u").map(pi => NamedRole(mkIri(pi)))

  private val n: List[Iri] =
    List("a", "b", "c", "d", "e", "f").map(mkIri)

  def draw: Vocabulary = Vocabulary(
    v.take(
      Random.between(
        minNumberOfVariables.min(v.size),
        maxNumberOfVariables.min(v.size + 1)
      )
    ).toSet,
    c.take(
      Random.between(
        minNumberOfConcepts.min(c.size),
        maxNumberOfConcepts.min(c.size + 1)
      )
    ).toSet,
    p.take(
      Random.between(
        minNumberOfProperties.min(p.size),
        maxNumberOfProperties.min(p.size + 1)
      )
    ).toSet,
    n.take(
      Random.between(
        minNumberOfNominals.min(n.size),
        maxNumberOfNominals.min(n.size + 1)
      )
    ).toSet
  )
