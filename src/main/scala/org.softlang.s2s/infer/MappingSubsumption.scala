package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._
import scala.collection.mutable.ListBuffer

class MappingSubsumption(
    a: AtomicPatterns,
    kb: Set[Axiom],
    debug: Boolean = false
)(implicit scopes: Scopes)
    extends Inference:

  private def thealgorithm(p1: (Set[Var], Set[AtomicPattern]), p2: (Set[Var], Set[AtomicPattern])): Unit = 
    if debug then 
      println("\nCase:")
      println("  Left: " + p1._1.mkString(", "))
      println("" + p1._2.mkString("\n"))
      println("  Right: " + p2._1.mkString(", "))
      println("" + p2._2.mkString("\n"))

    // Get the sets of variables as lists (fixed, arbitrary order).
    val v1 = p1._1.toList
    val v2 = p2._1.toList

    // generate possible mappings
    //   substitution for variables in p2 by (subset of) variables in p1
    // substitute (mappedWith) (subsumedBy)
    //   substitue variables in p2 by mapping
    // find diff
    //   ?
    // generate axioms (from diff)
    // prove axioms
    // if true: get (result) axioms from mapping

  def axioms: Set[Axiom] =

    // (1) Detect all components.
    val comps = a.components

    if debug then 
      println("\nKnowledge Base:")
      kb.foreach{ a =>
        println("  " + a.toString)
      }
      println("\nComponents:")
      comps.foreach{ c =>
        println("  " + c.toString)
      }

    // - Iterate all actual variable mappings
    // - Extend with fresh variables
    //    - how?
    // - Create axioms
    // - Decide validity of mapping by checking entailment of axioms

    // (2) Construct all extended components.
    //val extended = QueryExtensionOptimized(
    //  comps,
    //  shapes.map(_.inScopeS(Scope.Med))
    //).extended

    //if debug then
    //  println("")
    //  extended.foreach(println)

    //// (3) Generate component mappings and find subsumption.
    //(for
    //  // For each sub-pattern component (will be mapped).
    //  p1 <- comps
    //  // Test for each extended component, whether there is a mapping.
    //  pExt <- extended
    //yield componentMap(p1, pExt)).flatten.toSet
    
    // Brute-force approach for each combination:

    // - For each possible mapping
    //   - generate substituted pattern
    //   - find the 'diff' required for subsumption (set minus)
    //   - make constraint from this 'diff' (for variables)
    //   - for all 'targets' in original pattern (for variables)
    //     - test if target subsumed by constraint
    
    // Find combinations of components.
    for
      p1 <- comps
      p2 <- comps
    yield
      thealgorithm(p1, p2)

    if debug then println("\n")

    Set()

