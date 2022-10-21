package org.softlang.s2s.infer

import org.softlang.s2s.query._
import de.pseifer.shar.dl._

/** Generate the unique name assumption, given a set of atomic patterns. */
class UniqueNameAssumption(a: AtomicPatterns) extends Assumption(a):

  def axioms: Set[Axiom] =
    // For each pair of nominals.
    for
      o1 <- a.nominals
      o2 <- a.nominals
    // Generate axiom {𝑎} ⊓ {𝑏} = ⊥
    yield Equality(
      Intersection(NominalConcept(o1), NominalConcept(o2)),
      Bottom
    )
