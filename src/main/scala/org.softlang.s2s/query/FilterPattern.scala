package org.softlang.s2s.query

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole

import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Vocabulary

enum FilterPattern:
  case notC(v: Var, c: Iri) // x !C
  case notP(v: Var, p: Iri) // x !p

  /** Construct a vocabulary for the filter pattern (exclusind variables). */
  def removalVocabulary: Vocabulary =
    this match
      case notC(_, c) =>
        Vocabulary(
          variables = Set(),
          concepts = Set(NamedConcept(c)),
          properties = Set(),
          nominals = Set()
        )
      case notP(_, p) =>
        Vocabulary(
          variables = Set(),
          concepts = Set(),
          properties = Set(NamedRole(p)),
          nominals = Set()
        )

object FilterPattern:
  /** Get the joint vocabulary over all FilterPattern. */
  def removalVocabulary(fps: Set[FilterPattern]): Vocabulary =
    def vunion(l: Vocabulary, r: Vocabulary): Vocabulary =
      l.union(r)
    if fps.nonEmpty
    then fps.map(_.removalVocabulary).reduce(vunion)
    else Vocabulary.empty
