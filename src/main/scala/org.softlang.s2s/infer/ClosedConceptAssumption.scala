package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.core.isVariable
import org.softlang.s2s.query._

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

    val a2 =
      a1.flatMap(_ match
        case Equality(NamedConcept(v), r) if v.isVariable =>
          Set(
            Subsumption(NamedConcept(v), r.inScope(Scope.Pattern)),
            Subsumption(r.inScope(Scope.Input), NamedConcept(v))
          )
        case a => Set(a)
      )
    a2

  val leftScope = Scope.Pattern
  val rightScope = Scope.Pattern 

abstract class ClosedConceptAssumption(
    a: AtomicPatterns,
    useSubsumption: Boolean,
    includeVariableClosure: Boolean,
)(implicit scopes: Scopes)
    extends Scopeable:

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
      vcm.map(
        if useSubsumption 
        then Subsumption.apply 
        else Equality.apply).toSet
    )