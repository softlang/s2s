package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Vocabulary

import scala.util.Random

/** Generate all shapes over a vocabulary. */
class ShapeGenerator(voc: Vocabulary, optimize: Boolean):

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
          voc.concepts.flatMap { c =>
            Set(
              Existential(p, c),
              Existential(Inverse(p), c),
              Universal(p, c),
              Universal(Inverse(p), c)
            )
          }
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

  /** Filter, that matches all shapes entailed, given that all other shapes are
    * generated.
    */
  private def entailed(target: Concept, constraint: Concept): Boolean =
    target == constraint
      || (isNC(target) && isU(constraint))
      || (isE(target) && hasR(target) && isU(constraint) && hasR(constraint))
      || (isE(target) && hasIR(target) && isU(constraint) && hasIR(constraint))

  def generate: Set[SimpleSHACLShape] = for
    t <- generateTargets
    c <- generateConstraints
    if !optimize || !entailed(t, c)
  yield SimpleSHACLShape(Subsumption(t, c))
