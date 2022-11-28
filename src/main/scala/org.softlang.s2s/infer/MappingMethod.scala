package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

// - variable mapping
// - component equality modulo mapping
// - extraction of subsumptions

class MappingMethod(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape]
) extends Assumption(a):

  def axioms: Set[Axiom] =
    // (1) Find all components.
    val comp = a.components
    println("\n\nCOMP")
    comp.foreach(println) // DEBUG

    // (2) For each shape & component->variable, test if target.
    // If so, extend the pattern by translating the constraint.
    val t = comp.map((k, v) =>
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
    println("\n\nT")
    t.foreach(println) // DEBUG

    // (3) Generate variable mappings.
    // (3.1) Test subsumption for all combinations of two components.
    val ttt = for
      sub <- comp
      sup <- comp
    yield
      val subv = sub._2.toList.variables
      val supv = sup._2.toList.variables

      val mapping: Map[Var, Var] = Map() // TODO: generate all

      sub._2.toList.mappedWith(mapping).subsumedBy(sup._2.toList)

      (sub, sup)

    // Ultimately, only subset relationship.
    // def subsumedBy(other: AtomicPatterns, mapping: Map[Var, Var]): Boolean = false

    // (A) all variables of sub must be mapped to one element in sup
    // (B) elements in sup may be image of multiple elements in sub

    // sub.mappedWith(mapping).subsumedBy(sup)

    println("\n\nTTT")
    ttt.foreach(println) // DEBUG

    val l1 = List(1, 2)
    val l2 = List(3, 4)

    // (4) Find subsumption between components.

    Set()
