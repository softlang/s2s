package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.inScope
import org.softlang.s2s.core.isVariable
import org.softlang.s2s.query._

/** Test, whether equality with Scope.Input is allowed.
 *  This is allowed, if the variable dependency graph
 *  is acyclic.
 *  Slight hack for now. Before publication, 
 *  refine these inferences to stan-alone steps,
 *  that more clearly behave as expected. */
object EqualityCondition:
  private def hasEdge(s1: Set[Var], s2: Set[Var]): Boolean = 
    s1.intersect(s2).nonEmpty

  private def hasCycleOne(lst: List[Set[Var]]): Boolean = 
    lst.sliding(2).forall(l => hasEdge(l(0), l(1))) && hasEdge(lst.head, lst.last)

  private def hasCycle(lst: List[Set[Var]]): Boolean = 
    (3 to lst.size).map { i =>
      lst.take(i)
    }.exists(hasCycleOne)

  def isAllowed(a: AtomicPatterns): Boolean = 
    !a.map(_.variables)
      .filter(_.size == 2)
      .permutations
      .exists(hasCycle)

class ClosedConceptAssumptionTemplate(
    a: AtomicPatterns
)(implicit scopes: Scopes)
    extends ClosedConceptAssumption(a, true, false)(scopes):

  val leftScope = Scope.Template
  val rightScope = Scope.Template

class ClosedConceptAssumptionPattern(
    a: AtomicPatterns
)(implicit scopes: Scopes)
    extends ClosedConceptAssumption(a, false, true)(scopes):

  override protected def extendAxioms(axioms: Set[Axiom]): Set[Axiom] =
    val a1 =
      axioms.map(_ match
        case Equality(l @ NamedConcept(_), r) if a.concepts.contains(l) =>
          Equality(l, Intersection(l.inScope(Scope.Input), r))
        case a => a
      )

    val allowed = EqualityCondition.isAllowed(a)

    val a2 =
      a1.flatMap(_ match
        case Equality(NamedConcept(v), r) if v.isVariable =>
          Set(
            Subsumption(NamedConcept(v), r.inScope(Scope.Pattern)),
            Subsumption(r.inScope(Scope.Pattern), NamedConcept(v)),
            Subsumption(NamedConcept(v), r.inScope(Scope.Input)),
          ).union(
            if allowed then
              Set(Subsumption(r.inScope(Scope.Input), NamedConcept(v)))
            else Set())
        case a => Set(a)
      )
    a2

  val leftScope = Scope.Pattern
  val rightScope = Scope.Pattern

abstract class ClosedConceptAssumption(
    a: AtomicPatterns,
    useSubsumption: Boolean,
    includeVariableClosure: Boolean
)(implicit scopes: Scopes)
    extends Scopable:

  import AtomicPattern._

  /** Generate the CWA for non-variable concepts. */
  private def conceptClosure: Set[Axiom] = a.concepts.flatMap { c =>
    val rhs = a.flatMap {
      case LAC(is, d) if c == NamedConcept(d) => Set(NominalConcept(is))
      case VAC(vs, d) if c == NamedConcept(d) => Set(vs.asConcept)
      case _                                  => Set()
    }

    if rhs.isEmpty then Set()
    else Set(Equality(c, Concept.unionOf(rhs.toList)))
  }

  /** Generate the CWA for variables as a mapping. */
  private val variableClosure: Map[Concept, Concept] =
    a.variables.flatMap { x =>
      val rhs = a.flatMap {
        case VAC(y, io) if x == y => Set(NamedConcept(io))
        case VPL(y, ip, io) if x == y =>
          Set(Existential(NamedRole(ip), NominalConcept(io)))
        case LPV(is, ip, y) if x == y =>
          Set(Existential(Inverse(NamedRole(ip)), NominalConcept(is)))
        case VPV(y, ip, z) if x == y && x == z =>
          Set(
            Existential(NamedRole(ip), z.asConcept),
            Existential(Inverse(NamedRole(ip)), z.asConcept)
          )
        case VPV(y, ip, z) if x == y =>
          Set(Existential(NamedRole(ip), z.asConcept))
        case VPV(z, ip, y) if x == y =>
          Set(Existential(Inverse(NamedRole(ip)), z.asConcept))
        case _ => Set()
      }
      if rhs.isEmpty then Map()
      else Map(x.asConcept -> Concept.intersectionOf(rhs))
    }.toMap

  protected def prepareAxioms: Set[Axiom] =

    val vcm =
      if includeVariableClosure then variableClosure
      else Map()

    conceptClosure.union(
      vcm
        .map(
          if useSubsumption
          then Subsumption.apply
          else Equality.apply
        )
        .toSet
    )
