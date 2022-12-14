package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.rename
import org.softlang.s2s.query._

/** Generate the closed world assumption for a set of atomic patterns. */
class ClosedWorldAssumption(
    a: AtomicPatterns,
    // Closure for concepts.
    closeConcepts: Boolean,
    // Closure for properties.
    closeProperties: Boolean,
    // Closure for T.
    closeTop: Boolean,
    // Closure for literals {a}.
    closeLiterals: Boolean,
    // Use Subsumption instead of Equality.
    useSubsumption: Boolean,
    // Rename internal concepts.
    renameConcepts: Boolean,
    // The token appended when renaming.
    renameToken: String
) extends Assumption(a)
    with Renaming(renameConcepts, renameToken):

  import AtomicPattern._

  // Closure for concepts.
  private val conceptClosure: Set[Axiom] = a.concepts.flatMap { c =>
    val rhs = a.flatMap {
      case LAC(is, d) if c == NamedConcept(d) => Set(NominalConcept(is))
      case VAC(vs, d) if c == NamedConcept(d) => Set(vs.asConcept)
      case _                                  => Set()
    }
    if rhs.isEmpty then Set()
    else if useSubsumption then
      Set(Subsumption(rename(c), Concept.unionOf(rhs)))
    else Set(Equality(rename(c), Concept.unionOf(rhs)))
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

  // General closure T ⊑ X ⊔ ... ⊔ {a} ... ⊔ A ⊔ ...
  private val generalClosure =
    Subsumption(
      Top,
      Concept.unionOf(
        a.concepts.toList
          .union(a.nominals.toList.map(NominalConcept(_)))
          .union(a.variables.toList.map(_.asConcept))
      )
    )

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

  def axioms: Set[Axiom] =
    (if closeConcepts then conceptClosure else Set())
      .union(if closeProperties then propertyClosure else Set())
      .union(if closeProperties then inversePropertyClosure else Set())
      .union(if closeTop then Set(generalClosure) else Set())
      .union(if closeLiterals then literalsClosure else Set())
