package org.softlang.s2s.infer

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._
import de.pseifer.shar.reasoning.AxiomSet
import de.pseifer.shar.reasoning.HermitReasoner
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class ShapePropertySubsumption(
    // The input pattern.
    pattern: AtomicPatterns,
    // Input shapes.
    shapes: Set[SimpleSHACLShape],
    // Renaming token.
    renameToken: String
) extends PropertySubsumptionCommon(pattern)
    with Renaming(false, renameToken):

  import AtomicPattern._

  // TODO: Improve!

  // Rudamentary first version working only in very limited cases,
  // when properties are obviously the same (i.e., variables are not retricted).

  //

  def axioms: Set[Axiom] =
    patternConstraints.toSet.flatMap { x =>
      x match
        case (n, None)                   => Set()
        case (n, Some(c)) if c.size == 1 =>
          // If this variable occurrs only in this one pattern
          if pattern
              .filter(_.variables.contains(c.head._1))
              .size == 1 && pattern
              .filter(_.variables.contains(c.head._2))
              .size == 1
            // then this is the same role.
          then
            Set(
              RoleSubsumption(rename(n), n),
              RoleSubsumption(n, rename(n))
            )
          else 
            Set()
        case _ => Set()
    }
