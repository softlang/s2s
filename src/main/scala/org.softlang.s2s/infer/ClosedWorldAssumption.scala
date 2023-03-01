package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._

class ClosedWorldAssumptionForTemplate(
    a: AtomicPatterns,
    closeConcepts: Boolean,
    closeProperties: Boolean,
    closeLiterals: Boolean,
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends ClosedWorldAssumption(
      a,
      closeConcepts,
      closeProperties,
      closeLiterals,
      useSubsumption
    ):

  val leftScope = Scope.Template
  val rightScope = Scope.Pattern

class ClosedWorldAssumptionForPattern(
    a: AtomicPatterns,
    closeConcepts: Boolean,
    closeProperties: Boolean,
    closeLiterals: Boolean,
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends ClosedWorldAssumption(
      a,
      closeConcepts,
      closeProperties,
      closeLiterals,
      useSubsumption
    ):

  val leftScope = Scope.Pattern
  val rightScope = Scope.Input

/** Generate the closed world assumption for a set of atomic patterns. */
abstract class ClosedWorldAssumption(
    a: AtomicPatterns,
    // Closure for concepts.
    closeConcepts: Boolean,
    // Closure for properties.
    closeProperties: Boolean,
    // Closure for literals {a}.
    closeLiterals: Boolean,
    // Use Subsumption instead of Equality.
    useSubsumption: Boolean
)(implicit scopes: Scopes)
    extends Scopeable:

  import AtomicPattern._

  // Closure for concepts.
  private val conceptClosure: Set[Axiom] = a.concepts.flatMap { c =>
    val rhs = a.flatMap {
      case LAC(is, d) if c == NamedConcept(d) => Set(NominalConcept(is))
      case VAC(vs, d) if c == NamedConcept(d) => Set(vs.asConcept)
      case _                                  => Set()
    }
    if rhs.isEmpty then Set()
    else if useSubsumption then Set(Subsumption(c, Concept.unionOf(rhs)))
    else Set(Equality(c, Concept.unionOf(rhs)))
  }

  // Closure for properties.
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
      else if useSubsumption then
        Set(
          Subsumption(Existential(p, Top), Concept.unionOf(rhs))
        )
      else Set(Equality(Existential(p, Top), Concept.unionOf(rhs)))
    }

  // Closure for inverse properties.
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
      else if useSubsumption then
        Set(
          Subsumption(
            Existential(Inverse(p), Top),
            Concept.unionOf(rhs)
          )
        )
      else
        Set(
          Equality(Existential(Inverse(p), Top), Concept.unionOf(rhs))
        )
    }

  //// General closure T ⊑ X ⊔ ... ⊔ {a} ... ⊔ A ⊔ ...
  // private val generalClosure =
  //  Subsumption(
  //    Top,
  //    Concept.unionOf(
  //      a.concepts.toList
  //        .union(a.nominals.toList.map(NominalConcept(_)))
  //        .union(a.variables.toList.map(_.asConcept))
  //    )
  //  )

  private val literalsClosure: Set[Axiom] =
    a.nominals.flatMap { o1 =>
      val rhs = a.flatMap {
        case LAC(o2, c) if o1 == o2 => Set(NamedConcept(c))
        case LPV(o2, ip, vo) if o1 == o2 =>
          Set(Existential(NamedRole(ip), vo.asConcept))
        case LPL(o2, ip, o3) if o1 == o2 =>
          Set(Existential(NamedRole(ip), NominalConcept(o3)))
        case LPL(o2, ip, o3) if o1 == o3 =>
          Set(Existential(Inverse(NamedRole(ip)), NominalConcept(o3)))
        case _ => Set()
      }
      if rhs.isEmpty then Set()
      else Set(Subsumption(NominalConcept(o1), Concept.unionOf(rhs)))
    }

  protected def prepareAxioms: Set[Axiom] =
    (if closeConcepts then conceptClosure else Set())
      .union(if closeProperties then propertyClosure else Set())
      .union(if closeProperties then inversePropertyClosure else Set())
      // .union(if closeTop then Set(generalClosure) else Set())
      .union(if closeLiterals then literalsClosure else Set())

class AlternativeClosedWorldAssumption(
    a: AtomicPatterns,
    targetScope: Scope
)(implicit
    scopes: Scopes
):

  import AtomicPattern._

  private def fixScope(
      ax: Set[Axiom],
      leftScope: Scope,
      rightScope: Scope
  ): Set[Axiom] =
    ax.map(_ match
      case Subsumption(l, r) =>
        Subsumption(l.inScope(leftScope), r.inScope(rightScope))
      case Equality(l, r) =>
        Equality(l.inScope(leftScope), r.inScope(rightScope))
      case RoleSubsumption(l, r) =>
        RoleSubsumption(l.inScope(leftScope), r.inScope(rightScope))
      case a => a
    )

  private def general: Set[Axiom] = a.properties.flatMap { p =>
    Set(
      Subsumption(
        Existential(p.inScope(targetScope), Top),
        scopes.top(targetScope)
      ),
      Subsumption(
        Existential(Inverse(p.inScope(targetScope)), Top),
        scopes.top(targetScope)
      )
    )
  }

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

  def axioms: Set[Axiom] =
    val og = ClosedWorldAssumptionForPattern(a, false, true, false, true).axioms
    fixScope(og, targetScope, targetScope).union(general.union(specific))
