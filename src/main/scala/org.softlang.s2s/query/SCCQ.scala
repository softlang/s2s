package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.ShassTry
import org.softlang.s2s.core.UnsupportedQueryError
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Vocabulary
import scala.compiletime.ops.boolean

/** A list of atomic patterns. */
type AtomicPatterns = List[AtomicPattern]

extension (aps: AtomicPatterns)

  /** Get the variables in this set of patterns. */
  def variables: Set[Var] = aps.flatMap(_.variables).toSet

  /** Get the concepts in this set of patterns. */
  def concepts: Set[NamedConcept] = aps.flatMap(_.concepts).toSet

  /** Get the properties in this set of patterns. */
  def properties: Set[NamedRole] = aps.flatMap(_.properties).toSet

  /** Get the nominals in this set of patterns. */
  def nominals: Set[Iri] = aps.flatMap(_.nominals).toSet

  /** Rename in all atomic patterns in this set of patterns. */
  def rename(token: String): AtomicPatterns = aps.map(_.rename(token))

  /** Get the vocabulary of this set of patterns. */
  def vocabulary: Vocabulary =
    Vocabulary(aps.variables, aps.concepts, aps.properties, aps.nominals)

  /** Get all connected components of this set of patterns. */
  def components: Map[Set[Var], Set[AtomicPattern]] =
    // Function to find all components in a query pattern.
    def doComponents(
        p: List[AtomicPattern],
        partial: Map[Set[Var], Set[AtomicPattern]]
    ): Map[Set[Var], Set[AtomicPattern]] = p match
      // A next pattern remains.
      case head :: next =>
        // Build this entry, by...
        val thiss = partial.find(_._1.intersect(head.variables).nonEmpty) match
          // ...creating new component for new variables.
          case None =>
            partial + (head.variables -> Set(head))
          // ...or extending existing component.
          case Some(x) =>
            partial
              .removed(x._1) + (head.variables
              .union(x._1) -> (Set(head) ++ x._2))
        doComponents(next, thiss)
      // Processed all patterns.
      case Nil => partial
    doComponents(aps, Map())

  /** Apply a variable mapping to a set of patterns. */
  def mappedWith(mapping: Map[Var, Var]): AtomicPatterns =
    aps.map(_.mappedWith(mapping))

  /** Test, whether one pattern is subsumed by another. */
  def subsumedBy(other: AtomicPatterns): Boolean =
    aps.toSet.subsetOf(other.toSet)

/** Representation of a SCCQ (query) as template and pattern as List of
  * AtomicPattern.
  */
case class SCCQ(
    template: List[AtomicPattern],
    pattern: List[AtomicPattern]
) extends Showable:

  def show(implicit state: BackendState): String =
    "CONSTRUCT { " ++ template.map(_.show).mkString("", " . ", "") ++
      " } WHERE { " ++ pattern.map(_.show).mkString("", " . ", "") ++ " }"

  /** Get all variables in this query. */
  def variables: Set[Var] = template.variables.union(pattern.variables)

  /** Get all concepts in this query. */
  def concepts: Set[NamedConcept] = template.concepts.union(pattern.concepts)

  /** Get all properties in this query. */
  def properties: Set[NamedRole] = template.properties.union(pattern.properties)

  /** Get all nominals of this query. */
  def nominals: Set[Iri] = template.nominals.union(pattern.nominals)

  /** Get the vocabulary (variables, concepts, properties, nominals). */
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
