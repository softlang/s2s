package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class SubsumptionsFromMappings(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape],
    debug: Boolean = false
)(implicit scopes: Scopes)
    extends Inference:

  type Components = Map[Set[Var], Set[AtomicPattern]]

  /** All subsets of a required size. */
  private def subsets[T](s: Set[T], ofSize: Int): List[Set[T]] =
    if ofSize == 0 then List(Set())
    else if s.isEmpty then Nil
    else
      subsets(s.tail, ofSize - 1).map(Set(s.head) ++ _) ++ subsets(
        s.tail,
        ofSize
      )

  def axioms: Set[Axiom] =

    // (1)
    // Detect all components.
    val comps = a.components

    // (2)
    // For each shape & component->variable, test if it is a target.
    // If so, extend the pattern by translating the shape to a constraint.
    val extendedComps =
      SimpleSHACLShape.extendComponentsWithShapes(
        comps,
        // Map shapes to pattern scope first.
        shapes.map(_.inScope(Scope.Pattern)),
        a.depth
      )

    // (3)
    // Generate variable mappings and find subsumption.
    val ttt = for
      // The sub-query-component (note: will be the super-pattern!)
      sub <- extendedComps
      // The super-query-component (note: will be the sub-pattern!)
      sup <- comps
    yield
      // The complete (including temporary) variables in sub and sup.
      val subv = sub._2.toList.variables
      val supv = sup._2.toList.variables

      if debug then
        println(sup._1.mkString(",") ++ "\n | " ++ sup._2.mkString("\n | "))
      if debug then
        println(
          sub._1.mkString(",") ++ "\n | " ++ sub._2.mkString("\n | ") ++ "\n"
        )

      // C1 - All numbers of occurrences for variables.
      val c1 = for
        v <- subv
        i <- 0 to subv.size
      yield List.fill(i)(v)

      if debug then
        println(c1.size)
        println(c1)

      // C2 - All possible RHS (in terms of occurring variables).
      val c2 = subsets(c1.toList.flatten.toSet, supv.size)

      if debug then
        println(c2.size)
        println(c2)

      // C3 - Finally, all permutations for the RHS of the mapping.
      val c3 = c2
        .flatMap(_.toList.permutations)
        .toList

      if debug then
        println(c3.size)
        println(c3)

      val axioms =
        for ci <- c3
        yield
          // Mappings from sup variables to sub variables.
          val mapping = supv.zip(ci).toMap
          // If sup is subsumed by sub while using the current mapping
          if sup._2.toList.mappedWith(mapping).subsumedBy(sub._2.toList) then
            if debug then
              mapping.foreach((k, v) => print(k.v ++ " -> " ++ v.v ++ ", "))
              println()
            mapping
              // we can filter vacously satisfied subsumptions (i.e., x -> x)
              .filter(_ != _)
              // filter out any temporary variables
              .filter((x, y) => sup._1.contains(x) && sub._1.contains(y))
              // then generate the subsumption axioms (reverse of mapping).
              .map((x, y) => Subsumption(y.asConcept, x.asConcept))
          else Set()

      // Return all subsumption axioms.
      axioms.flatten

    ttt.toList.flatten.toSet
