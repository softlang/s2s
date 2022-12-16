package org.softlang.s2s.infer

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._
import de.pseifer.shar.reasoning.AxiomSet
import de.pseifer.shar.reasoning.HermitReasoner
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class PropertySubsumption(
    // The input pattern.
    pattern: AtomicPatterns,
    // Subsumption axioms for variables.
    subs: Set[Axiom],
    // Input template.
    template: AtomicPatterns
)(implicit scopes: Scopes)
    extends PropertySubsumptionCommon(pattern):

  // Constraints for template.
  private val templateConstraints = mkConstraints(template)

  // Reasoner for subsumptions.

  private val hermit = HermitReasoner.default
  hermit.addAxioms(AxiomSet(subs))

  /** Test, whether v1 is subsumed by v2 (according to subs). */
  private def subsumed(v1: Var, v2: Var): Boolean =
    hermit.prove(Subsumption(v1.asConcept, v2.asConcept))

  // Subsumtion property.

  /** Constraints for one property subsumed by constraints of another. */
  private def subsProperty(
      c1: Option[Constraints],
      c2: Option[Constraints]
  ): Boolean =
    if c1.isDefined && c2.isDefined then
      // For each constraint of LHS
      c1.get.forall(c1i =>
        // there must exist RHS constraints
        c2.get.exists(c2i =>
          // with subsumption for lhs (of property) and rhs (of property).
          subsumed(c1i._1, c2i._1) && subsumed(c1i._2, c2i._2)
        )
      )
    else false

  def axioms: Set[Axiom] =
    // Test for each property in pattern and in template
    pattern.properties.flatMap(p1 =>
      template.properties.flatMap(p2 =>
        // lookup constraints (Note: is never empty) and
        val c1 = patternConstraints.getOrElse(p1, Some(Set()))
        val c2 = templateConstraints.getOrElse(p2, Some(Set()))
        Set(
          // if the subsumption property holds one way
          if subsProperty(c1, c2) then Set(RoleSubsumption(p1, p2))
          else Set(),
          // or the other.
          if subsProperty(c2, c1) then Set(RoleSubsumption(p2, p1))
          else Set()
        ).flatten
      )
    )
