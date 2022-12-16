package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class DomainClosureAssumptionForTemplate(
    a: AtomicPatterns,
    eraseVariables: Boolean,
    approximateVariables: Boolean,
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends DomainClosureAssumption(
      a,
      eraseVariables,
      approximateVariables,
      useSubsumption
    )(scopes):

  val leftScope = Scope.Template
  val rightScope = Scope.Template

class DomainClosureAssumptionForPattern(
    a: AtomicPatterns,
    eraseVariables: Boolean,
    approximateVariables: Boolean,
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends DomainClosureAssumption(
      a,
      eraseVariables,
      approximateVariables,
      useSubsumption
    )(scopes):

  val leftScope = Scope.Pattern
  val rightScope = Scope.Input

/** Generate the domain closure assumption, given a set of atomic patterns. */
abstract class DomainClosureAssumption(
    a: AtomicPatterns,
    // Replace concept variables with T
    eraseVariables: Boolean,
    // Replace concept variables with an approximation.
    approximateVariables: Boolean,
    // Use subsumption instead of equality.
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends Scopeable:

  import AtomicPattern._

  /** Generate the DCA for variables as a mapping. */
  private val variableClosure: Map[Concept, Concept] =
    // For each variable in the set of atomic patterns
    a.variables.flatMap { x =>
      val rhs = a.flatMap {
        // check each pattern, whether it contains the variable.
        // If so, generate adomain domain closure concept a_i.
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

  /** Replace variable concepts by Top. */
  private val variablesToTop: Concept => Concept =
    case n @ NamedConcept(_) if variableClosure.keySet.contains(n) => Top
    case c                                                         => c

  /** Replace variable concepts by an approximation. */
  private val variablesToApproximation: Concept => Concept =
    case n @ NamedConcept(_) if variableClosure.keySet.contains(n) =>
      Concept.map(variablesToTop, variableClosure.getOrElse(n, default = Top))
    case c => c

  /** Update all variable concepts according to fn. */
  private def updateVariables(
      fn: Concept => Concept,
      vcs: Map[Concept, Concept]
  ): Map[Concept, Concept] =
    vcs.mapValues(c => Concept.map(fn, c)).toMap

  protected def prepareAxioms: Set[Axiom] =
    (if eraseVariables then updateVariables(variablesToTop, variableClosure)
     else if approximateVariables then
       updateVariables(variablesToApproximation, variableClosure)
     else variableClosure) .map(
      if useSubsumption then Subsumption.apply else Equality.apply
    ).toSet
