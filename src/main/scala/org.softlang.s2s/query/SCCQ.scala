package org.softlang.s2s.query

import org.softlang.s2s.core.{Var, Vocabulary, ShassTry, UnsupportedQueryError}
import de.pseifer.shar.core.{Showable, BackendState, Iri}
import de.pseifer.shar.dl.{NamedConcept, NamedRole}

type AtomicPatterns = List[AtomicPattern]

extension (aps: AtomicPatterns)
  def variables: Set[Var] = aps.flatMap(_.variables).toSet
  def concepts: Set[NamedConcept] = aps.flatMap(_.concepts).toSet
  def properties: Set[NamedRole] = aps.flatMap(_.properties).toSet
  def nominals: Set[Iri] = aps.flatMap(_.nominals).toSet

  def vocabulary: Vocabulary =
    Vocabulary(aps.variables, aps.concepts, aps.properties, aps.nominals)

case class SCCQ(
    template: List[AtomicPattern],
    pattern: List[AtomicPattern]
) extends Showable:

  def show(implicit state: BackendState): String =
    "CONSTRUCT { " ++ template.map(_.show).mkString("", " . ", "") ++
      " } WHERE { " ++ pattern.map(_.show).mkString("", " . ", "") ++ " }"

  def variables: Set[Var] = template.variables.union(pattern.variables)

  def concepts: Set[NamedConcept] = template.concepts.union(pattern.concepts)

  def properties: Set[NamedRole] = template.properties.union(pattern.properties)

  def nominals: Set[Iri] = template.nominals.union(pattern.nominals)

  def vocabulary: Vocabulary = template.vocabulary.union(pattern.vocabulary)

object SCCQ:

  /** Validate a SCCQ query. Raises an errors for invalid queries. Depending on
    * parameters (`rename`), attempts to fix query.
    */
  def validate(q: SCCQ, rename: Boolean = false): ShassTry[SCCQ] =
    def vocP = q.pattern.vocabulary
    def vocH = q.template.vocabulary

    if
      // Invalid: Variables in H, that do not occur in P.
      vocH.diff(vocP).variables.nonEmpty
      // Invalid: Shared concepts and properties in H and P.
      || vocP.intersect(vocH).concepts.nonEmpty
      || vocP.intersect(vocH).properties.nonEmpty
    then Left(UnsupportedQueryError(q))
    else Right(q)
