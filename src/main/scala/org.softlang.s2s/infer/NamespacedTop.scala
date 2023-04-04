package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

/** Generate definitions for the namespace specific Tops. */
class NamespacedTop(
    // The query pattern.
    pattern: AtomicPatterns,
    // The query template.
    template: AtomicPatterns,
    // If true, generate full definitions.
    // If false, set all specific Tops to Top.
    namespacedTop: Boolean
)(implicit scopes: Scopes)
    extends Inference:

  /** Definition of a specific Top-replacement over a set of atomic patterns. */
  private def aClosure(top: Concept, ap: AtomicPatterns): Set[Axiom] = Set(
    Equality(
      top,
      Concept.unionOf(
        ap.variables.toList
          .map(_.asConcept)
          .union(ap.nominals.toList.map(NominalConcept(_)))
      )
    )
  )

  /** Top definition for the pattern. */
  private def patternClosure: Set[Axiom] =
    aClosure(scopes.top(Scope.Pattern), pattern)

  /** Top definition for the template. */
  private def templateClosure: Set[Axiom] =
    aClosure(scopes.top(Scope.Template), template)

  /** General closure axioms, defining real Top and subsumption between pattern
    * and input.
    */
  private def generalClosure: Set[Axiom] = Set(
    // Relationship between input and pattern tops.
    Subsumption(scopes.top(Scope.Pattern), scopes.top(Scope.Input)),
    // Definition of the actual top concept.
    Equality(
      Top,
      Concept.unionOf(
        List(
          scopes.top(Scope.Input),
          scopes.top(Scope.Pattern),
          scopes.top(Scope.Template)
        )
      )
    )
  )

  def axioms: Set[Axiom] =
    if namespacedTop then
      generalClosure.union(templateClosure).union(patternClosure)
    else Set()
