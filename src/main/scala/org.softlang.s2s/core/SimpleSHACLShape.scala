package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern

/** A restricted, simple SHACL shape. */
class SimpleSHACLShape(axiom: Subsumption) extends SHACLShape(axiom):

  /** Put shape in specific scope. */
  def inScopeS(scope: Scope)(implicit scopes: Scopes): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.inScope(scope), axiom.d.inScope(scope))
    )

  /** Drop all scopes from this shape. */
  def dropScopeS(implicit scopes: Scopes): SimpleSHACLShape =
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

object SimpleSHACLShape:

  /** Test, whether Concept c is a valid constraint. */
  def validConstraint(c: Concept): Boolean =
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
    if SHACLShape.validTarget(axiom.c) && validConstraint(axiom.d) then
      Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))
