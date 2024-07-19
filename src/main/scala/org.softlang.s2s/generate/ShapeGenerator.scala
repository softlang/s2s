package org.softlang.s2s.generate

import de.pseifer.shar.dl._
import de.pseifer.shar.core.Iri
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.ShapeHeuristic
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Vocabulary
import org.softlang.s2s.core.dropScope
import org.softlang.s2s.core.Scopes
import scala.languageFeature.existentials

/** Generate all shapes over a vocabulary. */
class ShapeGenerator(
    voc: Vocabulary,
    heuristic: ShapeHeuristic,
    excludeTarget: Set[Iri] = Set()
)(implicit scopes: Scopes):

  /** Find a proxy axiom. */
  private def findProxy(s: String): Concept =
    val ci = NamedConcept(
      Iri.fromString("<https://github.com/softlang/s2s/" + s + ">").toOption.get
    )
    if voc.contains(ci) then findProxy(s ++ "'")
    else ci

  val proxy = findProxy("P")

  /** Generate all target queries (Concepts). */
  private def generateTargets: Set[Concept] =
    voc.concepts.toList
      .filter(c => !excludeTarget.contains(c.c))
      .concat(voc.properties.toList.flatMap { p =>
        if excludeTarget.contains(p.r.dropScope) then
          Set()
        else Set(Existential(p, Top), Existential(Inverse(p), Top))
      })
      .toSet

  /** Generate constraints (i.e., concepts). */
  private def generateConstraints: Set[Concept] = heuristic match
    case s:ShapeHeuristic.SimpleShapes => generateConstraintsSimple(s)
    case m:ShapeHeuristic.MediumProGS => generateConstraintsExtended(m)
    case _:ShapeHeuristic.AllShapes => generateConstraintsFull

  /** Generate additional constraints by ProGS heuristic. */
  private def generateConstraintsExtended(h: ShapeHeuristic.MediumProGS, level: Int = 0): Set[Concept] =

    // We use the following (syntactic) subset of ALCHOI axioms.
    // C = T | A | {a} | ∃r.C | ∃r-.C | ¬C | C ⊓ C
    //     ^^^^^^^^^^^^
    //                  ^^^^^^^^^^^^^^^^^^^
    //     leafs        composite
    // The remaining constraints are syntactic sugar.

    // If we reached maximum depth, return leaf constraints.
    if level >= h.depth then
      // A | ¬A | {a} | ¬{a}
      leafConstraints
    // Otherwise, construct a breadth of recursive constraints.
    else
      val c =
        generateConstraintsExtended(h, level + 1)
          .union(leafConstraints)
          .union(Set(Top))
          .map(Concept.simplify(_))
      // ∃r-.C | ∃r.C | C ⊓ C
      val composite =
        intersectionsOf(h.breadth, c)
        .union(existentialsOver(c, voc.properties))
      // C | ¬C
      composite
        .union(negationsOf(composite))
        .map(Concept.simplify(_))
        .filter(_ != Top)
        .filter(_ != Bottom)

  /** Construct all negations of concepts in c. */
  private def negationsOf(c: Set[Concept]): Set[Concept] =
    c.map(Complement(_))

  /** Construct all distinct intersection of 'breadth' elements. */
  private def intersectionsOf(breadth: Int, c: Set[Concept]): Set[Concept] =
    (for
      c <- c.toList.combinations(breadth)
    yield Concept.intersectionOf(c)).toSet

  /** Construct all existentials for all r over all c. */
  private def existentialsOver(c: Set[Concept], r: Set[NamedRole]): Set[Concept] =
    (for
      ci <- c
      ri <- r
    yield Set(
      Existential(ri, ci),
      Existential(Inverse(ri), ci))).flatten

  /** Generate all constraints allowed by heuristic. */
  private def generateConstraintsFull: Set[Concept] =

    // Generate all constraints, according to the following simplified syntax.
    // Nesting depth: Limited by factor n.

    val n = 1 // TODO: Take from query.

    // ϕ := A | ¬A | {a} | ¬{a}
    //    | ∃r.ϕ ⊓ ... ⊓ ϕ | ¬∃r.ϕ ⊔ ... ⊔ ϕ | ∃r.T | ¬∃r.T
    // r := p | p^-

    // Concepts and negated concepts.
    val cwn = andNotC

    // Closed existential (i.e., ∃r.T).
    val ce: Set[Concept] = voc.properties.toList
      .flatMap(r => Set(Existential(r, Top), Existential(Inverse(r), Top)))
      .toSet

    // Leaf-existential quantification (i.e., no recursive exists); depth 0.
    val leq = andNotC
      .union(ce)
      .subsets
      .toSet
      .flatMap(cs =>
        val ucs = Concept.unionOf(cs.toList)
        val ics = Concept.intersectionOf(cs.toList)
        voc.properties.toList.flatMap(r =>
          Set(
            Existential(r, ics),
            Existential(Inverse(r), ics),
            Complement(Existential(r, ucs)),
            Complement(Existential(Inverse(r), ucs))
          )
        )
      )

    leq.union(ce).union(cwn)

  /** Generate exactly simple SHACL constraints. */
  private def generateConstraintsSimple(s: ShapeHeuristic.SimpleShapes): Set[Concept] =
    voc.concepts.toList
      .concat(
        voc.properties.toList.flatMap { p =>
          val temp = voc.concepts.flatMap { c =>
            Set(
              Existential(p, c),
              Existential(Inverse(p), c),
              Universal(p, c),
              Universal(Inverse(p), c)
            )
          }
          if s.proxyFamily then
            temp.union(Set(Universal(p, proxy), Universal(Inverse(p), proxy)))
          else temp
        }
      )
      .toSet

  /** Generate all concepts and negated concepts. */
  private val andNotC: Set[Concept] =
    voc.concepts.toList.concat(voc.concepts.toList.map(Complement(_))).toSet

  /** All leaf constraints for (extended) ALCHOI axioms. */
  private val leafConstraints: Set[Concept] =
    val n: Set[Concept] = voc.nominals.map(NominalConcept(_))
    val c: Set[Concept] = voc.concepts.toSet
    n.union(n.map(Complement(_))).union(c).union(c.map(Complement(_)))

  /** Generate all closed existential and universal constraints. */
  private val closedQuantification: Set[Concept] =
    voc.properties.toList
      .flatMap(
        r => Set(Existential(r, Top),
                 Existential(Inverse(r), Top)))
      .toSet

  private def isNC(c: Concept): Boolean =
    c match
      case NamedConcept(_) => true
      case _               => false

  private def isU(c: Concept): Boolean =
    c match
      case Universal(_, _) => true
      case _               => false

  private def isE(c: Concept): Boolean =
    c match
      case Existential(_, _) => true
      case _                 => false

  private def hasIR(c: Concept): Boolean =
    c match
      case Universal(Inverse(_), _)   => true
      case Existential(Inverse(_), _) => true
      case _                          => false

  private def hasR(c: Concept): Boolean =
    c match
      case Universal(NamedRole(_), _)   => true
      case Existential(NamedRole(_), _) => true
      case _                            => false

  /** Matches all shapes entailed by other generated shapes. */
  private def entailed(target: Concept, constraint: Concept): Boolean =
    (isNC(target) && isU(constraint))
      || (isE(target) && hasR(target) && isU(constraint) && hasR(constraint))
      || (isE(target) && hasIR(target) && isU(constraint) && hasIR(constraint))

  /** Matches all tautologies. */
  private def tautology(target: Concept, constraint: Concept): Boolean =
    target == constraint

  /** Generate a set of shapes over a vocabulary, according to a heuristic. */
  def generate: Set[SHACLShape] =
    val constraints = generateConstraints
    for
      t <- generateTargets
      c <- constraints
      if (!heuristic.optimize || !entailed(t, c)) && !tautology(t, c)
    yield SHACLShape(Subsumption(t, c))
