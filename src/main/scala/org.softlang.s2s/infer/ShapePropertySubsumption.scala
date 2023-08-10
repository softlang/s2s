package org.softlang.s2s.infer

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._
import de.pseifer.shar.reasoning.AxiomSet
import de.pseifer.shar.reasoning.HermitReasoner
//import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class ShapePropertySubsumption(
    // The input pattern.
    pattern: AtomicPatterns
    // Input shapes.
    // shapes: Set[SimpleSHACLShape]
)(implicit scopes: Scopes)
    extends PropertySubsumptionCommon(pattern):

  import AtomicPattern._

  // TODO: Shapes targeting roles can widen unconstrained-ness.

  // Example: ∃:p.T ⊑ :A and the pattern "?x :p ?y . ?x a :A"

  // TODO: What about constraints that effectively do not change the result?

  def axioms: Set[Axiom] =
    patternConstraints.toSet.flatMap { x =>
      x match
        case (n, None)    => Set(RoleSubsumption(n, n.inScope(Scope.Input)))
        case (n, Some(c)) =>
          // Check that (all pairs) of variables are unconstrained.
          if c.forall(ci =>
              pattern
                .filter(_.variables.contains(ci._1))
                .size == 1 && pattern
                .filter(_.variables.contains(ci._2))
                .size == 1
              // Since loop is always tighter than the expressed shape:
                && ci._1 != ci._2
            )
            // then the role is unconstrained.
          then
            Set(
              RoleSubsumption(n, n.inScope(Scope.Input)),
              RoleSubsumption(n.inScope(Scope.Input), n)
            )
          // otherwise, the role is constrained.
          else Set(RoleSubsumption(n, n.inScope(Scope.Input)))
    }
