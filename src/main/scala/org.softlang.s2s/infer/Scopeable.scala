package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope

trait Scopeable(implicit scopes: Scopes) extends Inference:

  /** The scope of the LHS of axioms. */
  val leftScope: Scope

  /** The scope of RHS of axioms. */
  val rightScope: Scope

  /** Internal processing of the inference step. */
  protected def prepareAxioms: Set[Axiom]

  /** Apply scoping rules to the result axioms. */
  protected def scopeAxioms(ax: Set[Axiom]): Set[Axiom] =
    ax.map(_ match
      case Subsumption(l, r) =>
        Subsumption(l.inScope(leftScope), r.inScope(rightScope))
      case Equality(l, r) =>
        Equality(l.inScope(leftScope), r.inScope(rightScope))
      case RoleSubsumption(l, r) =>
        RoleSubsumption(l.inScope(leftScope), r.inScope(rightScope))
      case a => a
    )

  protected def scopedTop(ax: Set[Axiom]): Set[Axiom] =
    ax.map(scopes.replaceTop(_, leftScope, rightScope))

  // Combines the above steps (automatically) for Scopeable inference methods.
  def axioms: Set[Axiom] = scopedTop(scopeAxioms(prepareAxioms))
