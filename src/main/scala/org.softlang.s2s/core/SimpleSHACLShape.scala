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

  /** Rename concepts and properties of a Simple SHACL shape. */
  def renameProperties(token: String): SimpleSHACLShape =
    SimpleSHACLShape(
      Subsumption(
        axiom.c.renameIrisInProperties(token),
        axiom.d.renameIrisInProperties(token)
      )
    )

  /** True, if the constraint contains forall. */
  protected def isForallShape: Boolean = axiom.d match
    case Universal(_, _) => true
    case _               => false

  /** Test, whether `candidate` is a target of this shape `inPattern`. */
  protected def hasTarget(
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
  ): ShassTry[SimpleSHACLShape] =
    if validTarget(axiom.c) && validConstraint(axiom.d) then
      Right(SimpleSHACLShape(axiom))
    else Left(NotSimpleError(axiom))

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
          // The shapes, as a list where non-forall shapes come first (!)
          shapes.filter(!_.isForallShape).toList
            ++ shapes.filter(_.isForallShape).toList,
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
      shapes: List[SimpleSHACLShape],
      maxDepth: Int,
      chains: Set[Set[Var]],
      locked: Set[(Var, SimpleSHACLShape)]
  ): Set[AtomicPattern] = returning {
    // For all variables (currently added) and
    variables.flatMap(v =>
      // for all shapes, in order (non-forall first)
      shapes.flatMap(s =>
        // (A) if this step is allowed and
        if stepAllowed(v, s, component, maxDepth, chains, locked)
        then
          // this is a forall shape
          if s.isForallShape then
            // restart (!) after processing
            throwReturn {
              // by adding constraint for rhs of forall.
              recurWithForallShape(
                v,
                s,
                variables,
                component,
                shapes,
                maxDepth,
                chains,
                locked
              )
            }
          // this is not a forall shape
          else
            // restart (!) after processing
            throwReturn {
              // by expanding the component according to constraint.
              recurWithNormalShape(
                v,
                s,
                variables,
                component,
                shapes,
                maxDepth,
                chains,
                locked
              )
            }
        // (B) otherwise, keeo current component, thereby
        // continuing with the next shape/variable.
        else component
      )
    )
  }

  /** Decide, whether processing is allowed in this step. */
  private def stepAllowed(
      variable: Var,
      shape: SimpleSHACLShape,
      component: Set[AtomicPattern],
      maxDepth: Int,
      chains: Set[Set[Var]],
      locked: Set[(Var, SimpleSHACLShape)]
  ): Boolean =
    // If chaining (w.r.t. to fresh variables) does not exceed max
    minChain(chains, variable) <= maxDepth &&
      // and this is a forall shape
      !locked.contains((variable, shape)) &&
      // and the shape applies to this variable.
      shape.hasTarget(variable, component)
    // Then, this step is allowed.

  /** Construct the resursive step with forall shape. */
  private def recurWithForallShape(
      v: Var,
      s: SimpleSHACLShape,
      variables: Set[Var],
      component: Set[AtomicPattern],
      shapes: List[SimpleSHACLShape],
      maxDepth: Int,
      chains: Set[Set[Var]],
      locked: Set[(Var, SimpleSHACLShape)]
  ): Set[AtomicPattern] =

    // Get property and constraint. Must be guaranteed to be forall.
    val rc = (s.axiom.d match
      case Universal(r, c) => Some((r, c))
      case _               => None
    ).get

    // Get all the variables to be extended.
    val vars = component.flatMap(pat =>
      rc._1 match
        // If this is not inverse, then rhs of v p ?
        case NamedRole(ri) =>
          pat match
            case AtomicPattern.VPV(v1, p, v2) if v1 == v && p == ri => Some(v2)
            case _                                                  => None
        // If this is inverse, then lhs of ? p v
        case Inverse(NamedRole(ri)) =>
          pat match
            case AtomicPattern.VPV(v1, p, v2) if v2 == v && p == ri => Some(v1)
            case _                                                  => None
        // Inverse(Inverse(_)) etc. are not a valid shape constraint.
        case _ => None
    )

    // Extension (note, that _1 will be None, since rc._2 will never create fresh variables).
    val extension = vars.flatMap(constraintToAtomicPatterns(_, rc._2)._2)

    recursiveExtension(
      // No new variables.
      variables,
      // Add all extensions.
      component ++ extension,
      shapes,
      maxDepth,
      // No chaining of fresh variables occurs.
      chains,
      // Lock this variable/shape combination.
      locked ++ Set((v, s))
    )

  /** Construct the recursive step for normal shapes (non forall). */
  private def recurWithNormalShape(
      v: Var,
      s: SimpleSHACLShape,
      variables: Set[Var],
      component: Set[AtomicPattern],
      shapes: List[SimpleSHACLShape],
      maxDepth: Int,
      chains: Set[Set[Var]],
      locked: Set[(Var, SimpleSHACLShape)]
  ): Set[AtomicPattern] =
    val r = constraintToAtomicPatterns(v, s.axiom.d)
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
      // locked ++ r._1.map((_, s)).toSet.incl((v, s))
      // Lock this variable/shape combination.
      locked ++ Set((v, s))
    )

  /** Greate a new chain for a fresh variable. */
  private def newChain(chains: Set[Set[Var]], elem: Var): Set[Set[Var]] =
    chains ++ Set(Set(elem))

  /** Extend the chain w.r.t. to some element. */
  private def extendChain(
      chains: Set[Set[Var]],
      elem: Var,
      head: Var
  ): Set[Set[Var]] =
    chains.map(chain =>
      if chain.contains(head) then chain.incl(elem) else chain
    )

  /** Minimum chain for some variable. */
  private def minChain(chains: Set[Set[Var]], elem: Var): Int =
    chains.filter(_.contains(elem)).map(_.size).minOption.getOrElse(0)

  /** Generate patterns for this constraint, w.r.t. to targeted variable. */
  private def constraintToAtomicPatterns(
      target: Var,
      constraint: Concept
  ): (Option[Var], Set[AtomicPattern]) =
    import AtomicPattern._
    constraint match
      case NamedConcept(c) => (None, Set(VAC(target, c)))
      case Existential(NamedRole(p), NamedConcept(c)) =>
        val fresh = Var.fresh()
        (Some(fresh), Set(VPV(target, p, fresh), VAC(fresh, c)))
      case Existential(Inverse(NamedRole(p)), NamedConcept(c)) =>
        val fresh = Var.fresh()
        (Some(fresh), Set(VPV(fresh, p, target), VAC(fresh, c)))
      case _ => (None, Set())
