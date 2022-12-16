package org.softlang.s2s.infer

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._
import de.pseifer.shar.reasoning.AxiomSet
import de.pseifer.shar.reasoning.HermitReasoner
import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

abstract class PropertySubsumptionCommon(pattern: AtomicPatterns)
    extends Inference:

  // Constraints for pattern.
  protected val patternConstraints = mkConstraints(pattern)

  /** Constraints, as a tuple of occurring variables. */
  protected type Constraints = Set[(Var, Var)]

  /** Mapping from properties to Constraints. */
  protected type PropertyConstraints = Map[NamedRole, Option[Constraints]]

  /** Update property constraints, unless None. */
  private def update(
      cs: PropertyConstraints,
      property: NamedRole,
      constraint: (Var, Var)
  ): PropertyConstraints =
    cs.updated(
      property,
      (
        cs.getOrElse(property, Some(Set())).map(_ ++ Set(constraint))
      )
    )

  /** Set constraints to None for property. */
  private def invalidate(
      cs: PropertyConstraints,
      property: NamedRole
  ): PropertyConstraints = cs.updated(property, None)

  /** Add constraint for one AtomicPattern to PropertyConstraints. */
  private def addConstraints(
      constraints: PropertyConstraints,
      ap: AtomicPattern
  ): PropertyConstraints =
    import AtomicPattern._
    ap match
      case VPV(vs, ip, vo) => update(constraints, NamedRole(ip), (vs, vo))
      case LPV(is, ip, vo) => invalidate(constraints, NamedRole(ip))
      case VPL(vs, ip, io) => invalidate(constraints, NamedRole(ip))
      case LPL(is, ip, so) => invalidate(constraints, NamedRole(ip))
      case _               => constraints

  /** Build set of constraints for AtomicPatterns. */
  protected def mkConstraints(a: AtomicPatterns): PropertyConstraints =
    a.foldLeft(Map())(addConstraints)
