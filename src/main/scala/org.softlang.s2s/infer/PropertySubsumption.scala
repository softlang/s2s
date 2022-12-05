package org.softlang.s2s.infer

import de.pseifer.shar.core.Iri
import de.pseifer.shar.dl._
import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class PropertySubsumption(
    // The input pattern.
    pattern: AtomicPatterns,
    // Subsumption axioms for variables.
    subs: Set[Axiom],
    // Input template.
    template: AtomicPatterns
) extends Assumption(pattern):

  import AtomicPattern._

  // Constraints for pattern.
  private val patternConstraints = mkConstraints(pattern)

  // Constraints for template.
  private val templateConstraints = mkConstraints(template)

  // Definition/inferrence of constraints.

  type Constraints = (Set[Var], Set[Var])

  private def freshConstraints: Constraints = (Set(), Set())

  type PropertyConstraints = Map[NamedRole, Constraints]

  private def freshPropertyConstraints: PropertyConstraints = Map()

  private def update(
      cs: PropertyConstraints,
      property: NamedRole,
      left: Set[Var],
      right: Set[Var]
  ): PropertyConstraints =
    cs.updated(
      property,
      (
        cs.getOrElse(property, freshConstraints)._1 ++ left,
        cs.getOrElse(property, freshConstraints)._2 ++ right
      )
    )

  private def addConstraints(
      constraints: PropertyConstraints,
      ap: AtomicPattern
  ): PropertyConstraints = ap match
    case LPV(is, ip, vo) => update(constraints, NamedRole(ip), Set(), Set(vo))
    case VPL(vs, ip, io) => update(constraints, NamedRole(ip), Set(vs), Set())
    case VPV(vs, ip, vo) => update(constraints, NamedRole(ip), Set(vs), Set(vo))
    case LPL(is, ip, so) => update(constraints, NamedRole(ip), Set(), Set())
    case _               => constraints

  private def mkConstraints(a: AtomicPatterns): PropertyConstraints =
    a.foldLeft(freshPropertyConstraints)(addConstraints)

  // Subsumtion property.

  // TODO: Need to extend this definition to consider subsumption, etc.
  private def subsProperty(c1: Constraints, c2: Constraints): Boolean =
    c1 == c2
      && c1._1.size == 1 && c1._2.size == 1
      && c2._1.size == 1 && c2._2.size == 1

  def axioms: Set[Axiom] =
    // Test for each property in pattern and in template
    pattern.properties.flatMap(p1 =>
      template.properties.flatMap(p2 =>
        // lookup constraints (Note: is never empty) and
        val c1 = patternConstraints.getOrElse(p1, freshConstraints)
        val c2 = templateConstraints.getOrElse(p2, freshConstraints)
        Set(
          // if the subsumption property holds one way
          if subsProperty(c1, c2) then Set(RoleSubsumption(p1, p2)) else Set(),
          // or the other.
          if subsProperty(c2, c1) then Set(RoleSubsumption(p2, p1)) else Set()
        ).flatten
      )
    )
