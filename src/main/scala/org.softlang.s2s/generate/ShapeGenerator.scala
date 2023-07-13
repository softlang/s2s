package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import de.pseifer.shar.core.Iri
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Vocabulary

/** Generate all shapes over a vocabulary. */
class ShapeGenerator(
    voc: Vocabulary,
    optimize: Boolean,
    proxyFamily: Boolean,
    simple: Boolean
):

  /** Find a proxy axiom. */
  private def findProxy(s: String): Concept =
    val ci = NamedConcept(
      Iri.fromString("<https://github.com/softlang/s2s/" + s + ">").toOption.get
    )
    if voc.contains(ci) then findProxy(s ++ "'")
    else ci

  val proxy = findProxy("P")

  /** Generate all target queries (Concepts). */
  private def generateTargets: Set[Concept] =
    voc.concepts.toList
      .union(voc.properties.toList.flatMap { p =>
        Set(Existential(p, Top), Existential(Inverse(p), Top))
      })
      .toSet

  /** Generate all constraints (Concepts). */
  private def generateConstraints: Set[Concept] =
    voc.concepts.toList
      .union(
        voc.properties.toList.flatMap { p =>
          val temp = voc.concepts.flatMap { c =>
            Set(
              Existential(p, c),
              Existential(Inverse(p), c),
              Universal(p, c),
              Universal(Inverse(p), c)
            )
          }
          if proxyFamily then
            temp.union(Set(Universal(p, proxy), Universal(Inverse(p), proxy)))
          else temp
        }
      )
      .toSet

  private def isNC(c: Concept): Boolean =
    c match
      case NamedConcept(_) => true
      case _               => false

  private def isU(c: Concept): Boolean =
    c match
      case Universal(_, _) => true
      case _               => false

  private def isE(c: Concept): Boolean =
    c match
      case Existential(_, _) => true
      case _                 => false

  private def hasIR(c: Concept): Boolean =
    c match
      case Universal(Inverse(_), _)   => true
      case Existential(Inverse(_), _) => true
      case _                          => false

  private def hasR(c: Concept): Boolean =
    c match
      case Universal(NamedRole(_), _)   => true
      case Existential(NamedRole(_), _) => true
      case _                            => false

  /** Matches all shapes entailed by other generated shapes. */
  private def entailed(target: Concept, constraint: Concept): Boolean =
    (isNC(target) && isU(constraint))
      || (isE(target) && hasR(target) && isU(constraint) && hasR(constraint))
      || (isE(target) && hasIR(target) && isU(constraint) && hasIR(constraint))

  /** Matches all tautologies. */
  private def tautology(target: Concept, constraint: Concept): Boolean =
    target == constraint

  def generate: Set[SHACLShape] =
    if simple then generateSimple.toSet else generateGeneral.toSet

  private def generateSimple: List[SimpleSHACLShape] = (for
    t <- generateTargets
    c <- generateConstraints
    if (!optimize || !entailed(t, c)) && !tautology(t, c)
  yield SimpleSHACLShape(Subsumption(t, c))).toList

  private def generateGeneral: List[SHACLShape] =
    throw RuntimeException("Not implemented!")
