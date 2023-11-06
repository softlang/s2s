package org.softlang.s2s.infer

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl._
import org.softlang.s2s.query._
import org.softlang.s2s.core._
import scala.util.boundary

import scala.collection.mutable.{Set => Mset}
import de.pseifer.shar.core.Iri

// Algorithm (Sketch) TODO: Update Sketch

// 1. For each variable in P1, find all 'targets constraints'.
//    e.g., if x:A,(x,y):p then [A,p] for "x" and [p^-] for "y"
// 2. For each variable in P1, filter atomic patterns in P1, using shapes,
//    by condition "can be constructed with shapes".
//    2.0 Can this be done on a pattern-by-pattern basis?
//    2.1 Iterate each shape, and mark atoms involved in shapes.
//    2.1.1 Find variables in patterns that are targets,
//    2.1.2 Check whether they satisfy the constraints.
//    2.2 Remove all non-marked atoms.
//    2.3 Remove "dangling" atoms, not connected to Var.
//    Note Steps 2.1 and 2.2 can be pre-computed once (?)
// 3. Create a map Var -> ([Targets], [AtomicPattern]) from (2)
// 4. For each variable v in PExt, and for each elem in Map from (3),
//    if v is targeted by Targets, then instantiate the rhs
//    [AtomicPatterns] with v as Var and fresh variables for others.

class QueryExtensionOptimized(
    // All components of the pattern.
    components: List[(Set[Var], Set[AtomicPattern])],
    // The set of input shapes (scopes for Pattern scope).
    shapes: Set[SimpleSHACLShape]
):

  /** Type for predicates that identify whether a pattern is the target of some
    * shape. Var is the var we want to check for.
    */
  type IsTarget = (AtomicPattern, Var) => Boolean

  // Constructors for IsTarget predicates.

  private def targetToIsTarget(s: SimpleSHACLShape): IsTarget =
    s.axiom.c match
      case NamedConcept(c)                         => cIsTarget(c)
      case Existential(NamedRole(r), Top)          => pIsTarget(r)
      case Existential(Inverse(NamedRole(r)), Top) => piIsTarget(r)
      case _                                       => (_, _) => false

  private def cIsTarget(c: Iri): IsTarget = (a, vo) =>
    a match
      case AtomicPattern.VAC(vi, ci) if ci == c && vi == vo => true
      case _                                                => false

  private def pIsTarget(r: Iri): IsTarget = (a, vo) =>
    a match
      // case AtomicPattern.VPL(vi, ri, _) if ri == r && vi == vo => true
      case AtomicPattern.VPV(vi, ri, _) if ri == r && vi == vo => true
      case _                                                   => false

  private def piIsTarget(r: Iri): IsTarget = (a, vo) =>
    a match
      // case AtomicPattern.LPV(_, ri, vi) if ri == r && vi == vo => true
      case AtomicPattern.VPV(_, ri, vi) if ri == r && vi == vo => true
      case _                                                   => false

  /** A type for extending a set of atomic patterns with additional patterns for
    * a single target variable Var. Other variables are fresh.
    */
  type ApplyTemplate = Var => Set[AtomicPattern]

  /** A constructor for ApplyTemplate functions. */
  private def makeApplicationFn(
      // The focus variable.
      v: Var,
      // The input patterns.
      patterns: List[AtomicPattern]
  ): ApplyTemplate = vOther =>
    // Create a mapping from concrete variables to fresh ones,
    // and from variable 'v' to 'vOther', the ApplyTemplate argument.
    val vs = patterns.variables
    val mapping = vs.zip(List.fill(vs.size)(Var.fresh())).toMap
    patterns.mappedWith(mapping + (v -> vOther)).toSet

  /** For a single component, determine all patterns that can be constructed via
    * shapes, while also being potentially relevant to the algorithm (i.e., they
    * are part of the component and may be mapped.
    */
  private def makeTemplates(
      component: (Set[Var], Set[AtomicPattern])
  ): Set[(Set[IsTarget], ApplyTemplate)] =
    // We obtain one result for each variable, we call the focus of this template.
    component._1.flatMap { v =>
      shapes.flatMap { s =>
        val marked: Mset[AtomicPattern] = Mset()
        val targets: Mset[IsTarget] = Mset()
        // For each shape, and each variable, try to mark all patterns that can have impact on the final subsumption mapping.
        // shapes.foreach { s =>
        component._1.foreach { vi =>
          // If the variable is the current var v, also add the current shapes target. This identifies starting points for extending patterns.
          val exp = satisfied(s, v, component._2)
          if vi == v && exp.nonEmpty then targets.add(targetToIsTarget(s))
          marked.addAll(exp)
        }
        // }

        // Finally, we only need the connected component (since patterns were filtered, component is no longer connected) that contains the focus variable.
        if marked.isEmpty then Nil
        else
          val template =
            marked.toList.components.find(p => p._1.contains(v)).get._2.toList
          List((targets.toSet, makeApplicationFn(v, template)))
      }
    }.toSet

  /** Helper function for constructable, that translates a shape constraint to
    * atomic patterns, if and only if these patterns extend some relevant part
    * of the respective component.
    */
  private def satisfied(
      shape: SimpleSHACLShape,
      v: Var,
      component: Set[AtomicPattern]
  ): Set[AtomicPattern] =
    import AtomicPattern._

    def handle(
        vs: Var,
        v2: Var,
        p: AtomicPattern,
        ip: Iri,
        c: Iri
    ): Set[AtomicPattern] =
      if v2 == vs then
        val vn = Var.fresh()
        Set(VPV(vs, ip, vn), VAC(vn, c))
      else Set(p, VAC(v2, c))

    shape.axiom.d match
      case NamedConcept(c) => Set(VAC(v, c))
      case d @ Existential(NamedRole(r), NamedConcept(c)) =>
        component.flatMap { comp =>
          comp match
            // case p @ VPL(vs, ip, l) if ip == r && vs == v =>
            //   Set(p, LAC(l, c))
            case p @ VPV(vs, ip, v2) if ip == r && vs == v =>
              handle(vs, v2, p, ip, c)
            case _ => Set()
        }
      case Existential(Inverse(NamedRole(r)), NamedConcept(c)) =>
        component.flatMap { comp =>
          comp match
            // case p @ LPV(l, ip, vs) if ip == r && vs == v =>
            //   Set(p, LAC(l, c))
            case p @ VPV(v2, ip, vs) if ip == r && vs == v =>
              handle(vs, v2, p, ip, c)
            case _ => Set()
        }
      case _ => Set()

  /** Apply forall shapes for a component, by adding the right-hand side for the
    * appropriate properties, respecting shape targets.
    */
  private def applyForall(
      component: (Set[Var], Set[AtomicPattern])
  ): (Set[Var], Set[AtomicPattern]) =
    import AtomicPattern._

    def extendWithForall(v1: Var, v2: Var, role: Role): Set[AtomicPattern] =
      shapes.flatMap { s =>
        s.axiom.d match
          case Universal(r @ NamedRole(_), NamedConcept(c)) if r == role =>
            if s.hasTarget(v1, component._2) then Set(VAC(v2, c))
            else Set()

          case Universal(r @ Inverse(NamedRole(_)), NamedConcept(c))
              if r == role =>
            if s.hasTarget(v1, component._2) then Set(VAC(v2, c))
            else Set()
          case _ => Set()
      }

    component._1 -> component._2.flatMap { c =>
      c match
        case VPV(v1, p, v2) =>
          Set(c)
            .union(extendWithForall(v1, v2, NamedRole(p)))
            .union(extendWithForall(v2, v1, Inverse(NamedRole(p))))
        case _ => Set(c)
    }

  /** Apply all templates to a component. */
  private def extend(
      component: (Set[Var], Set[AtomicPattern]),
      templates: Set[(Set[IsTarget], ApplyTemplate)]
  ): (Set[Var], Set[AtomicPattern]) =
    // We iterate all patterns of the component, and try to apply each template
    // with each variable in the pattern as focus variable.
    // If IsTarget returns true (for any callback), then we simply add the
    // result of ApplyTemplate.
    val c = for
      c <- component._2
      t <- templates
      v <- c.variables
    yield if t._1.map(f => f(c, v)).exists(_ == true) then t._2(v) else Set()
    (component._1, component._2.union(c.flatten))

  /** Construct the set of all extended patterns. */
  val extended: List[(Set[Var], Set[AtomicPattern])] =
    // First, obtain all templates.
    val templates = components.toSet.flatMap(makeTemplates)
    // Then, we apply forall shapes once (on original variables) and then
    // extend with targets.
    components.map(applyForall).map(extend(_, templates))
