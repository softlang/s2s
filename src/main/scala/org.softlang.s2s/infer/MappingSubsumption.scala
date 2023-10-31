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

  def axioms: Set[Axiom] =

    // (1) Detect all components.
    val comps = a.components

    if debug then kb.foreach(println)
    if debug then comps.foreach(println)

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
    
    if debug then println("\n")

    Set()
