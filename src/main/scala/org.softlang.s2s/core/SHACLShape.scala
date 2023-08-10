package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern

/** An arbitrary SHACL shape, expressed by a Subsumption axiom. */
case class SHACLShape(val axiom: Subsumption) extends Showable {

  def show(implicit state: BackendState): String = axiom.show(state)

  /** Put shape in specific scope. */
  def inScope(scope: Scope)(implicit scopes: Scopes): SHACLShape =
    SHACLShape(
      Subsumption(axiom.c.inScope(scope), axiom.d.inScope(scope))
    )

  /** Drop all scopes from this shape. */
  def dropScope(implicit scopes: Scopes): SHACLShape =
    SHACLShape(
      Subsumption(axiom.c.dropScope, axiom.d.dropScope)
    )

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

  /** Convert to SimpleSHACLShape, if possible. */
  def toSimple: Option[SimpleSHACLShape] =
    SimpleSHACLShape.fromAxiom(axiom) match
      case Right(s) => Some(s)
      case Left(_)  => None

}

object SHACLShape:

  /** Test, whether Concept c is a valid target query. */
  def validTarget(c: Concept): Boolean =
    c match
      case Existential(NamedRole(_), Top)          => true
      case Existential(Inverse(NamedRole(_)), Top) => true
      case NamedConcept(_)                         => true
      // TBD: Node targets.
      case _ => false

  /** Test, whether Concept c is a valid constraint. */
  def validConstraint(c: Concept): Boolean = true

  /** Construct a shape from an axioms. */
  def fromAxiom(axiom: Subsumption): S2STry[SHACLShape] =
    if validTarget(axiom.c) then Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))
