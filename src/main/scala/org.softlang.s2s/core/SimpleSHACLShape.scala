package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern

case class SimpleSHACLShape(axiom: Subsumption) extends Showable:
  def show(implicit state: BackendState): String = axiom.show(state)

  def inScope(scope: Scope)(implicit scopes: Scopes): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.inScope(scope), axiom.d.inScope(scope))
    )

  def dropScope(implicit scopes: Scopes): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.dropScope, axiom.d.dropScope)
    )

  /** True, if the constraint contains universal quantification. */
  def isForallShape: Boolean = axiom.d match
    case Universal(_, _) => true
    case _               => false

  /** True, if the constraint contains existential quantification. */
  def isExistsShape: Boolean = axiom.d match
    case Existential(_, _) => true
    case _                 => false

  /** True, if the constraint contains existential quantification. */
  def isConceptShape: Boolean = axiom.d match
    case NamedConcept(_) => true
    case _               => false

  /** True, if the target contains existential quantification. */
  def hasExistentialTarget: Boolean = axiom.c match
    case Existential(_, _) => true
    case _                 => false

  /** Test, whether `candidate` is a target of this shape `inPattern`. */
  def hasTarget(
      candidate: Var,
      inPattern: Set[AtomicPattern]
  ): Boolean =
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

  /** Test, whether Concept c is a valid target query. */
  private def validTarget(c: Concept): Boolean =
    c match
      case Existential(Inverse(NamedRole(_)), Top) => true
      case Existential(NamedRole(_), Top)          => true
      case NamedConcept(_)                         => true
      case _                                       => false

  /** Test, whether Concept c is a valid constraint. */
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
  ): S2STry[SimpleSHACLShape] =
    if validTarget(axiom.c) && validConstraint(axiom.d) then
      Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))
