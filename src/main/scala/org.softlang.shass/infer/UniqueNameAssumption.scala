package org.softlang.shass.infer

import org.softlang.shass.query._
import de.pseifer.shar.dl._

/** Generate the unique name assumption, given a set of atomic patterns. */
class UniqueNameAssumption(a: AtomicPatterns) extends Assumption(a):

  def axioms: Set[Axiom] =
    // For each pair of nominals.
    for
      o1 <- a.nominals
      o2 <- a.nominals
    // Generate axiom {𝑎} ⊓ {𝑏} ⊑ ⊥
    yield Subsumption(
      Intersection(NominalConcept(o1), NominalConcept(o2)),
      Bottom
    )
