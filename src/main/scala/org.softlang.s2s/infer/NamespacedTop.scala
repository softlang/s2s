package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class NamespacedTop(
    pattern: AtomicPatterns,
    template: AtomicPatterns,
    namespacedTop: Boolean
)(implicit scopes: Scopes)
    extends Inference:

  private def aClosure(top: NamedConcept, ap: AtomicPatterns): Set[Axiom] = Set(
    Equality(
      top,
      Concept.unionOf(
        ap.variables.toList
          .map(_.asConcept)
          .union(ap.nominals.toList.map(NominalConcept(_)))
      )
    )
  )

  private def patternClosure: Set[Axiom] =
    aClosure(scopes.top(Scope.Pattern), pattern)

  private def templateClosure: Set[Axiom] =
    aClosure(scopes.top(Scope.Template), template)

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
    else
      Set(
        Equality(Top, scopes.top(Scope.Input)),
        Equality(Top, scopes.top(Scope.Pattern)),
        Equality(Top, scopes.top(Scope.Template))
      )
