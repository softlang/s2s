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
      sub <- extendedComps
      sup <- extendedComps
    yield
      // The complete (including temporary) variables in sub and sup.
      val subv = sub._2.toList.variables
      val supv = sup._2.toList.variables

      // If the subsumption candidate has more variables,
      // it can not be subsumed by the other component.
      if subv.size > supv.size then Nil
      else
        val diff = supv.size - subv.size

        // C1 - C3: Generation of all possible mappings, such that
        // all variables in sup are mapped to variables in sub,
        // with the additional constraint, that each variable
        // in sub must occur at least once.
        // (Otherwise, it can not be subsumed by sup.)

        // C1 - All numbers of occurrences for variables.
        val c1 = for 
          v <- subv
          i <- 1 to (1 + diff)
        yield List.fill(i)(v)

        // C2 - Restricted ocurrences to size of mapping (at least once).
        val c2 = c1.toList
          .combinations(subv.size)
          .map(_.flatten)
          .filter(_.size == supv.size)
          .filter(x => subv.forall(x.contains))
          .toList
        
        // C3 - Finally, all permutations for the RHS of the mapping.
        val c3 = c2.flatMap(_.permutations).toList

        val axioms = for
          ci <- c3
        yield
          // Mappings from sup variables to sub variables.
          val mapping = supv.zip(ci).toMap
          // If Sub is subsumed by sup while using the current mapping
          if sub._2.toList.subsumedBy(sup._2.toList.mappedWith(mapping)) then
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
