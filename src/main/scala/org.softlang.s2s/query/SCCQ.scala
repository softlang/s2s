package org.softlang.s2s.query

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Iri
import de.pseifer.shar.core.Showable
import de.pseifer.shar.dl.NamedConcept
import de.pseifer.shar.dl.NamedRole
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.S2STry
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

  /** Change the Scope of this set of patterns. */
  def inScope(scope: Scope)(implicit scopes: Scopes): AtomicPatterns =
    aps.map(_.inScope(scope))

  /** Get the vocabulary of this set of patterns. */
  def vocabulary: Vocabulary =
    Vocabulary(aps.variables, aps.concepts, aps.properties, aps.nominals)

  /** Get all connected components of this set of patterns. */
  def components: List[(Set[Var], Set[AtomicPattern])] =
    // Function to find all components in a query pattern.
    def doComponents(
        p: List[AtomicPattern],
        partial: Map[Set[Var], Set[AtomicPattern]]
    ): Map[Set[Var], Set[AtomicPattern]] = p match
      // A next pattern remains.
      case head :: next =>
        // Build this entry by finding all matches
        val ex = partial.filter(_._1.intersect(head.variables).nonEmpty)
        val thisS =
          // if there are none, create a new entry.
          if ex.isEmpty then partial + (head.variables -> Set(head))
          // otherwise, join existing ones.
          else
            // Get the unchanged rest of the components
            partial.filter(_._1.intersect(head.variables).isEmpty) +
              // and join the extended component(s) together.
              (ex.keySet.flatten.union(head.variables)
                -> (Set(head) ++ ex.values.flatten.toSet))
        doComponents(next, thisS)
      // Processed all patterns.
      case Nil => partial
    doComponents(aps, Map()).toList

  /** Apply a variable mapping to a set of patterns. */
  def mappedWith(mapping: Map[Var, Var]): AtomicPatterns =
    aps.map(_.mappedWith(mapping))

  /** Test, whether one pattern is subsumed by another. */
  def subsumedBy(other: AtomicPatterns): Boolean =
    aps.toSet.subsetOf(other.toSet)

  /** Find the maximum depth (connected variables) in this pattern. Currently
    * Estimate based on number of patterns with props. TODO: Improve
    * (performance slightly) by finding longest chain of variables.
    */
  def depth: Int =
    aps.filter(_.isPropertyPattern).size
  
  /** True, if the variable connectivity graph is cyclic. */
  def hasCyclicVCG: Boolean = 
    def hasEdge(s1: Set[Var], s2: Set[Var]): Boolean = 
      s1.intersect(s2).nonEmpty

    def hasCycleOne(lst: List[Set[Var]]): Boolean = 
      lst.sliding(2).forall(l => hasEdge(l(0), l(1))) && hasEdge(lst.head, lst.last)

    def hasCycle(lst: List[Set[Var]]): Boolean = 
      (3 to lst.size).map { i =>
        lst.take(i)
      }.exists(hasCycleOne)

    // Finally, apply these functions.
    !aps.map(_.variables)
      .filter(_.size == 2)
      .permutations
      .exists(hasCycle)

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
  def validate(
      q: SCCQ,
      renameToken: String
  ): S2STry[SCCQ] =
    def vocP = q.pattern.vocabulary
    def vocH = q.template.vocabulary

    // Invalid: Variables in H, that do not occur in P.
    if vocH.diff(vocP).variables.nonEmpty
    then
      Left(
        UnsupportedQueryError(q, details = "Template has undefined variables.")
      )
    else if vocP.contains(renameToken)
    then
      Left(
        UnsupportedQueryError(
          q,
          details =
            s"The query uses the restricted symbol $renameToken. Use --renameToken to change this symbol, or change the offending IRI."
        )
      )
    else Right(q)
