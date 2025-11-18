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
import org.softlang.s2s.query.GCORE

import scala.collection.mutable.Map as MMap

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

  /** Generate all node target queries (Concepts). */
  private def generateNodeTargets: Set[Concept] =
    voc.nodeLabels.union(voc.nodeKeys.map(key => Existential(key, Top)))

  /** Generate all edge target queries (Concepts). */
  private def generateEdgeTargets: Set[Concept] =
    voc.edgeLabels.union(voc.edgeKeys.map(key => Existential(key, Top)))

  /** Generate all target queries (Concepts). */
  private def generateTargets: Set[Concept] =
    voc.concepts.toList
      .filter(c => !excludeTarget.contains(c.c))
      .concat(voc.properties.toList.flatMap { p =>
        if excludeTarget.contains(p.r.dropScope) then Set()
        else Set(Existential(p, Top), Existential(Inverse(p), Top))
      })
      .toSet

  private def generateNodeConstraints: Set[Concept] = heuristic match
    case h: ShapeHeuristic.NovaProGS =>
      nodeConstraintsNova(h.breadth, h.depth, h.depth)
    case _ => generateConstraints

  private def generateEdgeConstraints: Set[Concept] = heuristic match
    case h: ShapeHeuristic.NovaProGS =>
      edgeConstraintsNova(h.breadth, h.depth, h.depth)
    case _ => generateConstraints

  /** Generate constraints (i.e., concepts). */
  private def generateConstraints: Set[Concept] = heuristic match
    case s: ShapeHeuristic.SimpleShapes => generateConstraintsSimple(s)
    case m: ShapeHeuristic.MediumProGS  => generateConstraintsExtended(m)
    case m: ShapeHeuristic.NovaProGS =>
      generateNodeConstraints.union(generateEdgeConstraints)
    case _: ShapeHeuristic.AllShapes => generateConstraintsFull

  private val ncache: MMap[Int, Set[Concept]] = MMap()

  private def nodeConstraintsNova(breadth: Int, depth: Int, level: Int) =
    ncache.get(level) match
      case None =>
        val c = generateNodeConstraintsNova(breadth, depth, level)
        ncache.addOne((level, c))
        c
      case Some(c) => c

  /** Generate node constraints for ProGS. */
  private def generateNodeConstraintsNova(
      breadth: Int,
      depth: Int,
      level: Int
  ): Set[Concept] =
    if level == 0 then
      // A | ¬A | T | kn.T
      val c = voc.nodeLabels
      val k: Set[Concept] = voc.nodeKeys.map(k => Existential(k, Top))
      c.union(c.map(Complement(_))).union(Set(Top)).union(k)
    else
      // Get all previous levels.
      val n =
        (0 until level)
          .map(i => nodeConstraintsNova(breadth, depth, i))
          .flatten
          .toSet
      // Build complements.
      val cn: Set[Concept] = n.map(Complement(_))
      // Get previous level of edge constraints.
      val e =
        (0 until level)
          .map(i => edgeConstraintsNova(breadth, depth, i))
          .flatten
          .toSet
          .filter(_ != Top)
      val en: Set[Concept] = e.flatMap(ec =>
        Set(
          Existential(GCORE.nodeToEdgeRole, ec),
          Existential(Inverse(GCORE.edgeToNodeRole), ec)
        )
      )

      val s = n.union(cn).union(en).map(Concept.simplify(_)).filter(_ != Bottom)
      val in =
        intersectionsOf(breadth, s)
          .map(Concept.simplify(_))
          .filter(_ != Bottom)
          .filter(_ != Top)
      in

  private val ecache: MMap[Int, Set[Concept]] = MMap()

  private def edgeConstraintsNova(breadth: Int, depth: Int, level: Int) =
    ecache.get(level) match
      case None =>
        val c = generateEdgeConstraintsNova(breadth, depth, level)
        ecache.addOne((level, c))
        c
      case Some(c) => c

  /** Generate edge constraints for ProGS. */
  private def generateEdgeConstraintsNova(
      breadth: Int,
      depth: Int,
      level: Int
  ): Set[Concept] =
    if level == 0 then
      // A | ¬A | T | kn.T
      val c = voc.edgeLabels
      val k: Set[Concept] = voc.edgeKeys.map(k => Existential(k, Top))
      c.union(c.map(Complement(_))).union(Set(Top)).union(k)
    else
      // Get all previous levels.
      val e =
        (0 until level)
          .map(i => edgeConstraintsNova(breadth, depth, i))
          .flatten
          .toSet
      // Build complements.
      val ce: Set[Concept] = e.map(Complement(_))
      // Get previous level of node constraints.
      val n =
        (0 until level)
          .map(i => nodeConstraintsNova(breadth, depth, i))
          .flatten
          .toSet
          .filter(_ != Top)
      val nn: Set[Concept] = n.flatMap(nc =>
        Set(
          Existential(GCORE.edgeToNodeRole, nc),
          Existential(Inverse(GCORE.nodeToEdgeRole), nc)
        )
      )

      val s = e.union(ce).union(nn).map(Concept.simplify(_)).filter(_ != Bottom)
      val in =
        intersectionsOf(breadth, s)
          .map(Concept.simplify(_))
          .filter(_ != Bottom)
          .filter(_ != Top)
      in

  /** Generate additional constraints by ProGS heuristic. */
  private def generateConstraintsExtended(
      h: ShapeHeuristic.MediumProGS,
      level: Int = 0
  ): Set[Concept] =
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
    (for c <- c.toList.combinations(breadth)
    yield Concept.intersectionOf(c)).toSet

  /** Construct all existentials for all r over all c. */
  private def existentialsOver(
      c: Set[Concept],
      r: Set[NamedRole]
  ): Set[Concept] =
    (for
      ci <- c
      ri <- r
    yield Set(Existential(ri, ci), Existential(Inverse(ri), ci))).flatten

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
  private def generateConstraintsSimple(
      s: ShapeHeuristic.SimpleShapes
  ): Set[Concept] =
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
      .flatMap(r => Set(Existential(r, Top), Existential(Inverse(r), Top)))
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

  /** Matches all tautologies. */
  private def negated(target: Concept, constraint: Concept): Boolean =
    constraint match
      case Complement(c) => target == c
      case _             => false

  /** Generate a set of shapes over a vocabulary, according to a heuristic. */
  def generate: Set[SHACLShape] =
    heuristic match
      case ShapeHeuristic.NovaProGS(_, _, _) =>
        val nodeConstraints = generateNodeConstraints
        val nt = for
          t <- generateNodeTargets
          c <- nodeConstraints
          if (!heuristic.optimize || !entailed(t, c)) && !tautology(
            t,
            c
          ) && !negated(t, c) && c != Top
        yield SHACLShape(Subsumption(t, c))

        val edgeConstraints = generateEdgeConstraints
        val et = for
          t <- generateEdgeTargets
          c <- edgeConstraints
          if (!heuristic.optimize || !entailed(t, c)) && !tautology(
            t,
            c
          ) && !negated(t, c) && c != Top
        yield SHACLShape(Subsumption(t, c))

        // Return the union of both sets.
        nt.union(et)

      case _ =>
        val constraints = generateConstraints
        for
          t <- generateTargets
          c <- constraints
          if (!heuristic.optimize || !entailed(t, c)) && !tautology(t, c)
        yield SHACLShape(Subsumption(t, c))
