package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern

case class SimpleSHACLShape(axiom: Subsumption) extends Showable:
  def show(implicit state: BackendState): String = axiom.show(state)

  /** Rename concepts and properties of a Simple SHACL shape. */
  def rename(token: String): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.renameIris(token), axiom.d.renameIris(token))
    )

  /** Generate atomic patterns for this shapes constraint, w.r.t. to a targeted
    * variable.
    */
  def constraintToAtomicPatterns(target: Var): Set[AtomicPattern] =
    import AtomicPattern._
    axiom.d match
      case NamedConcept(c) => Set(VAC(target, c))
      case Existential(NamedRole(p), NamedConcept(c)) =>
        val fresh = Var.fresh()
        Set(VPV(target, p, fresh), VAC(fresh, c))
      case Existential(Inverse(NamedRole(p)), NamedConcept(c)) =>
        val fresh = Var.fresh()
        Set(VPV(fresh, p, target), VAC(fresh, c))
      case _ => Set()

  /** Test, whether `candidate` is a target of this shape `inPattern`. */
  def isTarget(candidate: Var, inPattern: Set[AtomicPattern]): Boolean =
    import AtomicPattern._
    axiom.c match
      case NamedConcept(c) =>
        inPattern.exists(_ match
          case VAC(v, d) => v == candidate && d == c
          case _         => false
        )
      case Existential(NamedRole(r), Top) =>
        inPattern.exists(_ match
          case VPL(v, p, _) => v == candidate && p == r
          case VPV(v, p, _) => v == candidate && p == r
          case _            => false
        )
      case Existential(Inverse(NamedRole(r)), Top) =>
        inPattern.exists(_ match
          case VPV(_, p, v) => v == candidate && p == r
          case _            => false
        )
      case _ => false

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
