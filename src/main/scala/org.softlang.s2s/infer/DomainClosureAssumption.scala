package org.softlang.s2s.infer

import org.softlang.s2s.query._
import de.pseifer.shar.dl._

/** Generate the domain closure assumption, given a set of atomic patterns. */
class DomainClosureAssumption(a: AtomicPatterns) extends Assumption(a):

  def axioms: Set[Axiom] =
    import AtomicPattern._
    // For each variable in the set of atomic patterns
    a.variables.flatMap { x =>
      val rhs = a.flatMap { ap =>
        // check each pattern, whether it contains the variable.
        // If so, generate adomain domain closure concept a_i.
        ap match
          case VAC(y, io) if x == y => Set(NamedConcept(io))
          case VPL(y, ip, io) if x == y =>
            Set(Existential(NamedRole(ip), NominalConcept(io)))
          case LPV(is, ip, y) if x == y =>
            Set(Existential(Inverse(NamedRole(ip)), NominalConcept(is)))
          case VPV(y, ip, z) if x == y && x == z =>
            Set(
              Existential(NamedRole(ip), z.asConcept),
              Existential(Inverse(NamedRole(ip)), z.asConcept)
            )
          case VPV(y, ip, z) if x == y =>
            Set(Existential(NamedRole(ip), z.asConcept))
          case VPV(z, ip, y) if x == y =>
            Set(Existential(Inverse(NamedRole(ip)), z.asConcept))
          case _ => Set()
      }
      // Close variable concept X ≡ a_1 ⊓ ... ⊓ a_n.
      if rhs.isEmpty then Set()
      else Set(Equality(x.asConcept, Concept.intersectionOf(rhs)))
    }
