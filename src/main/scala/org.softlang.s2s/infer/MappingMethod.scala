package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.query._

class MappingMethod(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape]
) extends Assumption(a):

  def axioms: Set[Axiom] = Set()
