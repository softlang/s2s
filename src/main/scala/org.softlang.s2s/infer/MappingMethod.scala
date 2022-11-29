package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class MappingMethod(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape]
) extends Assumption(a):

  def axioms: Set[Axiom] =

    // (1)
    // Detect all components.
    val comps = a.components

    // (2) 
    // For each shape & component->variable, test if it is a target.
    // If so, extend the pattern by translating the shape to a constraint.
    val extendedComps = comps.map((k, v) =>
      (
        k,
        k.flatMap(ki =>
          v ++ shapes.flatMap(s =>
            // For each variable/shape combination (ki, s) test if is target
            if s.isTarget(ki, v) then
              // and extend the pattern if so.
              s.constraintToAtomicPatterns(ki)
            else
              // Otherwise, do not add anything.
              Set()
          )
        )
      )
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

      // C1 - All numbers of occurrences for variables.
      val c1 = for 
        v <- subv
        i <- 0 to subv.size
      yield List.fill(i)(v)

      // C2 - All possible RHS (in terms of occurring variables).
      val c2 = c1.toList
        .flatten.toSet
        // All subsets
        .subsets
        // of the required size.
        .filter(_.size == supv.size)
      
      // C3 - Finally, all permutations for the RHS of the mapping.
      val c3 = c2
        .flatMap(_.toList.permutations)

      val axioms = for
        ci <- c3
      yield
        // Mappings from sup variables to sub variables.
        val mapping = supv.zip(ci).toMap
        // If sup is subsumed by sub while using the current mapping
        if sup._2.toList.mappedWith(mapping).subsumedBy(sub._2.toList) then
          mapping
            // we can filter vacously satisfied subsumptions (i.e., x -> x)
            .filter(_ != _)
            // filter out any temporary variables
            .filter((x,y) => sup._1.contains(x) && sub._1.contains(y))
            // then generate the subsumption axioms (reverse of mapping).
            .map((x,y) => Subsumption(y.asConcept, x.asConcept))
        else
          Set()

      // Return all subsumption axioms.
      axioms.flatten

    ttt.toList.flatten.toSet
