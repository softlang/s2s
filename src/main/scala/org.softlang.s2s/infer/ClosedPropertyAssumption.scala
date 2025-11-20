package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._
import org.softlang.s2s.query.GCORE.Key
import org.softlang.s2s.core.Var

class ClosedPropertyAssumption(
    a: AtomicPatterns,
    targetScope: Scope,
    input: AlgorithmInput
)(implicit
    scopes: Scopes
) extends Scopable:

  import AtomicPattern._

  val leftScope = targetScope

  val rightScope = targetScope

  // TODO Fix? Correct?
  val template: Boolean = true // targetScope == Scope.Out

  private def axiomize(
      all: Set[Concept],
      role: NamedRole,
      cs: List[(Concept, Concept)],
      inverse: Boolean
  ): Set[Axiom] =
    val maps = cs.groupBy((_._1))
    all.flatMap(c =>
      // Construct p as either the role, or Inverse(role).
      val p = if inverse then Inverse(role) else role
      // Left-hand side: Existential quantification.
      val ex = Existential(p.inScope(targetScope), c)

      if maps.contains(c) then
        // Construct the RHS from all occurrences in the map (i.e., query).
        val rhs = maps(c).map(_._2)

        // If this is a extended query, construct additional Vx/Ox concepts.
        if template && input.isECCQ then
          val vx =
            // If inverse, use gen objects instead of variable concepts.
            if inverse then
              if Key.isNodeKey(role.r) then
                input.nodeVariablesNonBlank.flatMap(v =>
                  v.asRoleComponent(input.filters, role).toSet
                )
              else if Key.isEdgeKey(role.r) then
                input.edgeVariablesNonBlank.flatMap(v =>
                  v.asRoleComponent(input.filters, role).toSet
                )
              else Set()
            // If not inverse, use variable concepts.
            else if Key.isNodeKey(role.r) then
              input.nodeVariablesNonBlank.map(_.asConcept)
            else if Key.isEdgeKey(role.r) then
              input.edgeVariablesNonBlank.map(_.asConcept)
            else Set()
          List(
            // ∃𝑝.C ⊑ D1 ⊔ ... ⊔ Dn ⊔ ... ⊔ V1 ⊔ ... ⊔ Vm (or O1 ⊔ ... ⊔ Om for inverse)
            Subsumption(ex, Concept.unionOf(rhs ++ vx)),
            // D1 ⊔ ... ⊔ Dn ⊑ ∃𝑝.C
            Subsumption(Concept.unionOf(rhs), ex)
          )

        // Otherwise (not ECCQ), just construct the equality of ex and rhs.
        else
          // ∃𝑝.C ≡ D1 ⊔ ... ⊔ Dn
          List(Equality(ex, Concept.unionOf(rhs)))
      else Nil
    )

  private def specific: Set[Axiom] = a.properties.flatMap { p =>
    // Find all the occurrences of p in regular cases.
    val vu = a.flatMap {
      case LPL(u, ip, v) if NamedRole(ip) == p =>
        Set((NominalConcept(v), NominalConcept(u)))
      case VPL(u, ip, v) if NamedRole(ip) == p =>
        Set((NominalConcept(v), u.asConcept))
      case LPV(u, ip, v) if NamedRole(ip) == p =>
        Set((v.asConcept, NominalConcept(u)))
      case VPV(u, ip, v) if NamedRole(ip) == p =>
        Set((v.asConcept, u.asConcept))
      case _ => Set()
    }

    // Find all the occurrences of p in inverse cases.
    val vui = a.flatMap {
      case LPL(v, ip, u) if NamedRole(ip) == p =>
        Set((NominalConcept(v), NominalConcept(u)))
      case VPL(v, ip, u) if NamedRole(ip) == p =>
        Set((v.asConcept, NominalConcept(u)))
      case LPV(v, ip, u) if NamedRole(ip) == p =>
        Set((NominalConcept(v), u.asConcept))
      case VPV(v, ip, u) if NamedRole(ip) == p =>
        Set((v.asConcept, u.asConcept))
      case _ => Set()
    }

    // All nominal and variable concepts of the query.
    val all = a.nominals
      .map(NominalConcept(_))
      .toList
      .concat(a.variables.map(_.asConcept).toList)
      .toSet

    // Create the subsumption/equality axioms.
    axiomize(all, p, vu, inverse = false)
      .union(axiomize(all, p, vui, inverse = true))
  }

  /** Construct closure over properties. */
  private val propertyClosure: Set[Axiom] =
    a.properties.flatMap { p =>
      // Find all the occurrences of property p and their left-hand side.
      val rhs = a.flatMap {
        case LPL(is, ip, io) if p.r == ip =>
          Set(
            Intersection(
              NominalConcept(is),
              Existential(p, NominalConcept(io))
            )
          )
        case VPL(vs, ip, io) if p.r == ip =>
          Set(
            Intersection(
              vs.asConcept,
              Existential(p, NominalConcept(io))
            )
          )
        case LPV(is, ip, vo) if p.r == ip =>
          Set(
            Intersection(
              NominalConcept(is),
              Existential(p, vo.asConcept)
            )
          )
        case VPV(vs, ip, vo) if p.r == ip =>
          Set(
            Intersection(
              vs.asConcept,
              Existential(p, vo.asConcept)
            )
          )
        case _ => Set()
      }
      if rhs.isEmpty then Set()
      else if template && input.isECCQ then
        val vxp =
          if Key.isNodeKey(p.r) then
            input.nodeVariablesNonBlank.flatMap(
              _.asRoleComponent(input.filters, p).toSet
            )
          else if Key.isEdgeKey(p.r) then
            input.edgeVariablesNonBlank.flatMap(
              _.asRoleComponent(input.filters, p).toSet
            )
          else Set()

        Set(
          // ∃𝑝.⊤ ≡ (C1 ⊓ ∃𝑝.D1) ⊔ ... ⊔ (C1 ⊓ ∃𝑝.D1) ⊔ Vxp1 ⊔ ... ⊔ Vxpn
          Equality(Existential(p, Top), Concept.unionOf(rhs ++ vxp))
        )
      else
        Set(
          // ∃𝑝.⊤ ≡ (C1 ⊓ ∃𝑝.D1) ⊔ ... ⊔ (C1 ⊓ ∃𝑝.D1)
          Equality(Existential(p, Top), Concept.unionOf(rhs))
        )
    }

  /** Construct closure over inverse properties. */
  private val inversePropertyClosure: Set[Axiom] =
    a.properties.flatMap { p =>
      // Find all the occurrences of property p and their right-hand side (inverse).
      val rhs = a.flatMap {
        case LPL(is, ip, io) if p.r == ip =>
          Set(
            Intersection(
              NominalConcept(io),
              Existential(Inverse(p), NominalConcept(is))
            )
          )
        case VPL(vs, ip, io) if p.r == ip =>
          Set(
            Intersection(
              NominalConcept(io),
              Existential(Inverse(p), vs.asConcept)
            )
          )
        case LPV(is, ip, vo) if p.r == ip =>
          Set(
            Intersection(
              vo.asConcept,
              Existential(Inverse(p), NominalConcept(is))
            )
          )
        case VPV(vs, ip, vo) if p.r == ip =>
          Set(
            Intersection(
              vo.asConcept,
              Existential(Inverse(p), vs.asConcept)
            )
          )
        case _ => Set()
      }
      if rhs.isEmpty then Set()
      else if template && input.isECCQ then
        val make = (x: Var) =>
          for (pc, oc) <- x.asRoleObjectComponent(input.filters, p)
          yield Intersection(oc, Existential(Inverse(p), pc))
        val vxp =
          if Key.isNodeKey(p.r) then input.nodeVariablesNonBlank.flatMap(make)
          else if Key.isEdgeKey(p.r) then
            input.edgeVariablesNonBlank.flatMap(make)
          else Set()
        Set(
          Equality(Existential(Inverse(p), Top), Concept.unionOf(rhs ++ vxp))
        )
      else
        Set(
          // ∃𝑝-.⊤ ≡ (C1 ⊓ ∃𝑝-.D1) ⊔ ... ⊔ (C1 ⊓ ∃𝑝-.D1)
          Equality(Existential(Inverse(p), Top), Concept.unionOf(rhs))
        )
    }

  def prepareAxioms: Set[Axiom] =
    propertyClosure.union(inversePropertyClosure)

  override def addAxioms: Set[Axiom] = specific
