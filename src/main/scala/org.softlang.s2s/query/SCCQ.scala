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

    val data = aps
      .map(_.variables)
      .filter(_.size == 2)

    // Determine whether an edge exists.
    def hasEdge(s1: Set[Var], s2: Set[Var]): Boolean =
      s1.intersect(s2).nonEmpty

    val visited = scala.collection.mutable.Set[Int]()

    def hasCycleDFS(node: Int, parent: Int): Boolean =
      visited.add(node)

      (0 until data.size).exists { neighbor =>
        if neighbor != node && hasEdge(data(node), data(neighbor)) then
          if !visited.contains(neighbor) then hasCycleDFS(neighbor, node)
          else neighbor != parent // cycle detected
        else false
      }

    val result = (0 until data.size).exists { start =>
      !visited.contains(start) && hasCycleDFS(start, -1)
    }
    result

/** Extended query data.
  *
  * @param filter
  *   Filter conditions.
  * @param nodeVariables
  *   Variables that are node variables.
  * @param edgeVariables
  *   Variables that are edge variables.
  */
class ECCQ(
    val filter: Set[FilterPattern] = Set(),
    val nodeVariables: Set[Var] = Set(),
    val edgeVariables: Set[Var] = Set()
)

/** Representation of a SCCQ (query) as template and pattern as List of
  * AtomicPattern.
  */
class SCCQ(
    val template: List[AtomicPattern],
    val pattern: List[AtomicPattern],
    // Extension: ECCQ queries with filter patterns.
    val eccq: Option[ECCQ] = None
) extends Showable:

  def show(implicit state: BackendState): String =
    val to = template.map(_.show)
    val po = pattern.map(_.show)

    // Take only node and edge variables into consideration, not any variables generated for
    // properties by the conversion from G-CORE to SPARQL.
    // Then Generate two fresh variables, for each variable v.
    val patternVariables =
      eccq
        .map(e => e.nodeVariables.union(e.edgeVariables))
        .getOrElse(Set())
        .map(v => (v -> (Var.fresh(), Var.fresh())))
        .toMap
        .view
        .filterKeys(v => !v.isBlank)
        .toMap

    // Separator for lines in output.
    val sep = " .\n    "

    val p =
      if isECCQ then po ++ patternVariables.map(additionalTriples)
      else po
    val t =
      if isECCQ then
        // Filter for variables that occur in the template.
        val tv =
          patternVariables.view.filterKeys(v => template.variables.contains(v))
        to ++ tv.map(additionalTriples)
      else to
    val f =
      if isECCQ then
        // Filter only nodes.
        val pvn = patternVariables
          .filter((v, _) =>
            eccq.map(e => e.nodeVariables).getOrElse(Set()).contains(v)
          )
        // Filter only edges.
        val pve = patternVariables.filter((v, _) =>
          eccq.map(e => e.edgeVariables).getOrElse(Set()).contains(v)
        )
        // Filters from GCORE filter expressions.
        eccq
          .map(e => e.filter.map(makeFilter(_, patternVariables)))
          .getOrElse(Nil)
        // Filters for meta edges and nodes.
          ++ pvn.map(metaFilterNode)
          ++ pve.map(metaFilterEdge)
      else Nil

    t.mkString("CONSTRUCT {\n    ", " .\n    ", "\n}")
      ++ (p ++ f).mkString(" WHERE {\n    ", " .\n    ", "\n}")

  // Generate a triple pattern from a variable v, and a (fresh) property and object variable.
  private def additionalTriples(v: Var, po: (Var, Var)): String =
    s"${v.showNB} ${po._1.showNB} ${po._2.showNB}"

  private def makeFilter(f: FilterPattern, mv: Map[Var, (Var, Var)]): String =
    f match
      case FilterPattern.notC(v, c) =>
        val vc = mv(v)
        s"FILTER ( ${vc._2.showNB} !=  ${c.encode} )"
      case FilterPattern.notP(v, p) =>
        val vc = mv(v)
        s"FILTER ( ${vc._1.showNB} != ${p.encode} )"

  /** Make filter patterns for the generic nodes. */
  private def metaFilterNode(v: Var, po: (Var, Var)): String =
    s"FILTER ( ${po._1.showNB} != ${GCORE.nodeToEdgeIri.encode} )"

  /** Make filter patterns for the generic edges. */
  private def metaFilterEdge(v: Var, po: (Var, Var)): String =
    s"FILTER ( ${po._1.showNB} != ${GCORE.edgeToNodeIri.encode} )"

  /** Get all variables in this query. */
  def variables: Set[Var] = template.variables.union(pattern.variables)

  /** Get all concepts in this query. */
  def concepts: Set[NamedConcept] = template.concepts.union(pattern.concepts)

  /** Get all properties in this query. */
  def properties: Set[NamedRole] = template.properties.union(pattern.properties)

  /** Get all nominals of this query. */
  def nominals: Set[Iri] = template.nominals.union(pattern.nominals)

  /** Get the vocabulary (variables, concepts, properties, nominals). */
  def vocabulary: Vocabulary =
    template.vocabulary
      .union(pattern.vocabulary)
      .diff(GCORE.removeVoc)

  /** True if this is an extended query. */
  def isECCQ: Boolean = eccq.isDefined

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
