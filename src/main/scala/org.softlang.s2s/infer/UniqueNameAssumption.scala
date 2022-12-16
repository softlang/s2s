package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.query._

/** Generate the unique name assumption, given a set of atomic patterns. */
class UniqueNameAssumption(a: AtomicPatterns) extends Inference:

  def axioms: Set[Axiom] =
    // For each pair of nominals.
    for
      o1 <- a.nominals
      o2 <- a.nominals
      if o1 != o2
    // Generate axiom {𝑎} ⊓ {𝑏} = ⊥
    yield Equality(
      Intersection(NominalConcept(o1), NominalConcept(o2)),
      Bottom
    )
