package org.softlang.shass.infer

import org.softlang.shass.query._
import de.pseifer.shar.dl._

/** Generate the closed world assumption for a set of atomic patterns. */
class ClosedWorldAssumption(a: AtomicPatterns) extends Assumption(a):

  import AtomicPattern._

  // Closure for concepts.
  private val conceptClosure: Set[Axiom] = a.concepts.flatMap { c =>
    val rhs = a.flatMap { ap =>
      ap match
        case LAC(is, d) if c == NamedConcept(d) => Set(NominalConcept(is))
        case VAC(vs, d) if c == NamedConcept(d) => Set(vs.asConcept)
        case _                                  => Set()
    }
    if rhs.isEmpty then Set() else Set(Equality(c, Concept.unionOf(rhs)))
  }

  // Closure for properties.
  private val propertyClosure: Set[Axiom] = 
    a.properties.flatMap { p =>
      val rhs = a.flatMap { ap =>
        ap match
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
      else Set(Equality(Existential(p, Top), Concept.unionOf(rhs)))
    }

  private val inversePropertyClosure: Set[Axiom] = 
    a.properties.flatMap { p =>
      val rhs = a.flatMap { ap =>
        ap match
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
      else Set(Equality(Existential(Inverse(p), Top), Concept.unionOf(rhs)))
    }

  // General closure T ⊑ X ⊔ ... ⊔ {a} ... ⊔ A ⊔ ...
  private val generalClosure = 
    Subsumption(Top, Concept.unionOf(
      a.concepts.toList.union(
        a.nominals.toList.map(NominalConcept(_))).union(
          a.variables.toList.map(_.asConcept))))

  def axioms: Set[Axiom] = 
    conceptClosure
      .union(propertyClosure)
      .union(inversePropertyClosure)
      .union(Set(generalClosure))
