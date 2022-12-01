package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query.AtomicPattern
import org.softlang.s2s.query.depth
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

  /** Extend components with a set of shapes. */
  def extendComponentsWithShapes(
      components: Map[Set[Var], Set[AtomicPattern]],
      shapes: Set[SimpleSHACLShape]
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
          patterns.toList.depth
        )
      )
    )

  /** Recursive extension of a single component with a set of shapes. */
  private def recursiveExtension(
      variables: Set[Var],
      component: Set[AtomicPattern],
      shapes: Set[SimpleSHACLShape],
      maxDepth: Int,
      depth: Int = 0,
      locked: Set[(Var, SimpleSHACLShape)] = Set()
  ): Set[AtomicPattern] = returning {
    // For each variable/shape
    variables.flatMap(v =>
      shapes.flatMap(s =>
        // If the variable is a target of the shape (and not already processed).
        if depth <= maxDepth && !locked.contains((v, s)) && s
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
              // Increase depth, if chain of two fresh variables occurred.
              if r._1.isDefined && r._1.get.isFresh && v.isFresh then depth + 1
              else depth,
              // Locked variable/shape combinations, including the current step and fresh variables produced for this shape.
              locked ++ r._1.map((_, s)).toSet.incl((v, s))
            )
          }
          // Note: Need to start again, since target relationship may be changed by shapes.
        else component
      )
    )
  }

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
