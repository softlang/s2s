package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._
import scala.collection.mutable.ListBuffer

// TODO: This is rather inefficient. Optimizations:
// - Generate only relevant expansions more aggressively.
// - Oprimize detection of subsumptions via more efficient data structure.
//   - e.g., turn components into primitive array, check for equality.
class SubsumptionsFromMappings(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape],
    debug: Boolean = false
)(implicit scopes: Scopes)
    extends Inference:

  type Components = Map[Set[Var], Set[AtomicPattern]]

  private def dbug(s: String): Unit =
    if debug then println(s)

  /** All subsets of a required size. */
  private def subsets[T](s: Set[T], ofSize: Int): List[Set[T]] =
    if ofSize == 0 then List(Set())
    else if s.isEmpty then Nil
    else
      subsets(s.tail, ofSize - 1).map(Set(s.head) ++ _) ++ subsets(
        s.tail,
        ofSize
      )

  def axioms: Set[Axiom] =

    // (1)
    // Detect all components.
    val comps = a.components

    // (2)
    // For each shape & component->variable, test if it is a target.
    // If so, extend the pattern by translating the shape to a constraint.
    val extendedComps =
      SimpleSHACLShape.extendComponentsWithShapes(
        comps,
        // Map shapes to pattern scope first.
        shapes.map(_.inScope(Scope.Pattern)),
        a.depth
      )

    // (3)
    // Generate variable mappings and find subsumption.
    val ttt = for
      // The extended component.
      pext <- extendedComps
      // The sub-pattern component (will be mapped).
      p1 <- comps
    yield
      // The complete (including temporary) variables in pext and p1.
      val pextv = pext._2.toList.variables
      val p1v = p1._2.toList.variables

      dbug("---- ---- ----")

      dbug(
        "[P1] " ++ p1._1.mkString(",") ++ "\n | " ++ p1._2.mkString("\n | ")
      )
      dbug(
        "[Pext] " ++ pext._1.mkString(",") ++ "\n | " ++ pext._2.mkString(
          "\n | "
        ) ++ "\n"
      )

      dbug("[P1 Variables]\n  " ++ p1v.mkString(","))

      // C1 - All numbers of occurrences for variables.
      val c1 = for
        v <- pextv
        i <- 0 to p1v.size
      yield List.fill(i)(v)

      // C2 - All possible RHS (in terms of occurring variables).
      // Must have at least one non-fresh variable to infer relevant mapping.
      val c2 =
        subsets(c1.toList.flatten.toSet, p1v.size).filter(_.exists(!_.isFresh))

      val axioms = ListBuffer[Subsumption]()

      def test(ci: List[Var]): Unit =
        // Mappings from P1 variables to Pext variables.
        val mapping = p1v.zip(ci).toMap
        // If P1 is subsumed by Pext under the current mapping:
        if p1._2.toList.mappedWith(mapping).subsumedBy(pext._2.toList) then
          dbug("[Mappings]")
          mapping.foreach((k, v) => dbug(k.v ++ " -> " ++ v.v ++ ", "))
          mapping
            // we can filter vacously satisfied subsumptions (i.e., x -> x)
            .filter(_ != _)
            // filter out any temporary variables
            .filter((x, y) => p1._1.contains(x) && pext._1.contains(y))
            // then generate the subsumption axioms (reverse of mapping).
            .map((x, y) => Subsumption(y.asConcept, x.asConcept))
            // Add to axioms.
            .foreach(axioms.addOne)

      for ci <- c1.toList do test(ci)
      for c <- c2 do for ci <- c.toList.permutations do test(ci)

      // Return all subsumption axioms.
      axioms

    ttt.toList.flatten.toSet
