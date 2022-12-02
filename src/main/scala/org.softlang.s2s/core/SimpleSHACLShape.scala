package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern
import scala.util.control.NonLocalReturns.*

case class SimpleSHACLShape(axiom: Subsumption) extends Showable:
  def show(implicit state: BackendState): String = axiom.show(state)

  /** Rename concepts and properties of a Simple SHACL shape. */
  def rename(token: String): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(axiom.c.renameIris(token), axiom.d.renameIris(token))
    )

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

  /** Extend components with a set of shapes. */
  def extendComponentsWithShapes(
      components: Map[Set[Var], Set[AtomicPattern]],
      shapes: Set[SimpleSHACLShape],
      maxDepth: Int
  ): Map[Set[Var], Set[AtomicPattern]] =
    components.map((vars, patterns) =>
      (
        vars,
        patterns ++ recursiveExtension(
          // Initial variables (for this pattern).
          vars,
          // The pattern.
          patterns,
          // The shapes.
          shapes,
          // Maximum depth required (in case of mutually recursive shapes).
          maxDepth,
          // Initial chains (empty).
          Set(),
          // Initially locked variables (empty).
          Set()
        )
      )
    )

  /** Recursive extension of a single component with a set of shapes. */
  private def recursiveExtension(
      variables: Set[Var],
      component: Set[AtomicPattern],
      shapes: Set[SimpleSHACLShape],
      maxDepth: Int,
      chains: Set[List[Var]],
      locked: Set[(Var, SimpleSHACLShape)]
  ): Set[AtomicPattern] = returning {
    // For each variable/shape
    variables.flatMap(v =>
      shapes.flatMap(s =>
        // TODO: Should we *stop* if this chain exists, or rather, only stop for this variable?! => alternatively, solved by sorting vars?

        // If the variable is a target of the shape (and not already processed).
        if maxChain(chains) <= maxDepth && !locked.contains((v, s)) && s
            .isTarget(v, component)
        then
          // Add the shapes constraint as pattern
          val r = constraintToAtomicPatterns(v, s)
          // Recur with extended variables, component and (v,s) locked.
          throwReturn {
            recursiveExtension(
              // Variables, including any fresh ones from this step.
              variables ++ r._1,
              // The extended component with added patterns.
              component ++ r._2,
              // The original set of shapes.
              shapes,
              maxDepth,
              // Extend tracking of maximum chains.
              if r._1.isDefined && r._1.get.isFresh then
                if v.isFresh then extendChain(chains, r._1.get, v)
                else newChain(chains, r._1.get)
              else chains,
              // Locked variable/shape combinations, including the current step and fresh variables produced for this shape.
              locked ++ r._1.map((_, s)).toSet.incl((v, s))
              //locked.incl((v, s)) // TODO: Need to lock r._1 or not?
            )
          }
          // Note: Need to start again, since target relationship may be changed by shapes.
        else component
      )
    )
  }

  /** Greate a new chain for a fresh variable. */
  private def newChain(chains: Set[List[Var]], elem: Var): Set[List[Var]] =
    chains ++ Set(List(elem))

  /** Extend the chain w.r.t. to some element. */
  private def extendChain(
      chains: Set[List[Var]],
      elem: Var,
      head: Var
  ): Set[List[Var]] =
    chains.map(chain => if chain.head == head then elem :: chain else chain)

  /** Longest chain of variables. */
  private def maxChain(chains: Set[List[Var]]): Int =
    chains.map(_.size).maxOption.getOrElse(0)

  /** Generate atomic patterns for this shapes constraint, w.r.t. to a targeted
    * variable.
    */
  private def constraintToAtomicPatterns(
      target: Var,
      shape: SimpleSHACLShape
  ): (Option[Var], Set[AtomicPattern]) =
    import AtomicPattern._
    shape.axiom.d match
      case NamedConcept(c) => (None, Set(VAC(target, c)))
      case Existential(NamedRole(p), NamedConcept(c)) =>
        val fresh = Var.fresh()
        (Some(fresh), Set(VPV(target, p, fresh), VAC(fresh, c)))
      case Existential(Inverse(NamedRole(p)), NamedConcept(c)) =>
        val fresh = Var.fresh()
        (Some(fresh), Set(VPV(fresh, p, target), VAC(fresh, c)))
      case _ => (None, Set())

  /** Construct a shape from an axioms. */
  def fromAxiom(
      axiom: Subsumption
  ): ShassTry[SimpleSHACLShape] =
    if validTarget(axiom.c) && validConstraint(axiom.d) then
      Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))
