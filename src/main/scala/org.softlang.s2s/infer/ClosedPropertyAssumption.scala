package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class ClosedPropertyAssumption(
    a: AtomicPatterns,
    targetScope: Scope
)(implicit
    scopes: Scopes
) extends Scopable:

  import AtomicPattern._

  val leftScope = targetScope

  val rightScope = targetScope

  private def axiomize(
      all: Set[Concept],
      role: Role,
      cs: List[(Concept, Concept)]
  ): Set[Axiom] =
    val maps = cs.groupBy((_._1))
    all.flatMap(c =>
      val ex = Existential(role.inScope(targetScope), c)
      if maps.contains(c) then
        val rhs = maps(c).map(_._2)
        Some(Equality(ex, Concept.unionOf(rhs)))
      else None
    )

  private def specific: Set[Axiom] = a.properties.flatMap { p =>
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

    val all = a.nominals
      .map(NominalConcept(_))
      .toList
      .union(a.variables.map(_.asConcept).toList)
      .toSet
    axiomize(all, p, vu).union(axiomize(all, Inverse(p), vui))
  }

  private val propertyClosure: Set[Axiom] =
    a.properties.flatMap { p =>
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
      else
        Set(
          Subsumption(Existential(p, Top), Concept.unionOf(rhs))
        )
    }

  private val inversePropertyClosure: Set[Axiom] =
    a.properties.flatMap { p =>
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
      else
        Set(
          Subsumption(
            Existential(Inverse(p), Top),
            Concept.unionOf(rhs)
          )
        )
    }

  def prepareAxioms: Set[Axiom] =
    propertyClosure.union(inversePropertyClosure)

  override def addAxioms: Set[Axiom] = specific
