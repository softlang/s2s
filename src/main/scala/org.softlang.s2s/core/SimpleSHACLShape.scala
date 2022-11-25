package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._

case class SimpleSHACLShape(axiom: Subsumption) extends Showable:
  def show(implicit state: BackendState): String = axiom.show(state)

  /** Rename concepts and properties of a Simple SHACL shape. */
  def rename(token: String): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.renameIris(token), axiom.d.renameIris(token))
    )

object SimpleSHACLShape:

  private def validTarget(c: Concept): Boolean =
    c match
      case Existential(Inverse(NamedRole(_)), Top) => true
      case Existential(NamedRole(_), Top)          => true
      case NamedConcept(_)                         => true
      case _                                       => false

  private def validConstraint(c: Concept): Boolean =
    c match
      case NamedConcept(_)                                     => true
      case Existential(NamedRole(_), NamedConcept(_))          => true
      case Existential(Inverse(NamedRole(_)), NamedConcept(_)) => true
      case Universal(NamedRole(_), NamedConcept(_))            => true
      case Universal(Inverse(NamedRole(_)), NamedConcept(_))   => true
      case _                                                   => false

  /** Construct a shape from an axioms. */
  def fromAxiom(
      axiom: Subsumption
  ): ShassTry[SimpleSHACLShape] =
    if validTarget(axiom.c) && validConstraint(axiom.d) then
      Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))
