package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.query._

// Just a test!
class PropertySubsumption(
    a: AtomicPatterns,
    other: AtomicPatterns
) extends Assumption(a):

  import AtomicPattern._

  // - Mapping approach. Mapping from properties to properties, such that the patterns are the same (w.r.t. to variables)
  // - Also consider variable subsumption?
  // - Problem: Requires (at least) the DCA and input shapes after all.

  // - Any way of doing it without requiering DCA/S_in?

  def axioms: Set[Axiom] =
    a.properties.flatMap { p1 =>
      other.properties.flatMap(p2 =>
        Set(RoleSubsumption(p1, p2), RoleSubsumption(p2, p1))
      )
    }
