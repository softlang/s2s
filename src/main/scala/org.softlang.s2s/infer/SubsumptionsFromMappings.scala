package org.softlang.s2s.infer

import de.pseifer.shar.dl._
import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.core.Var
import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.core.inScope
import org.softlang.s2s.query._
import scala.collection.mutable.ListBuffer

/* Algorithm:

  For each combination of P1 pattern and Pext pattern,
  we construct a list where for each P1-variable there
  is a potential mapping candidate, or a blank.

  Then, we iterate all combinations of these lists and find any,
  where the variable lists for each component are agreeable.
  That is, would map to the same variable (or are blank).

  This produces a very small set of candidates. We still need to
  check for subsumption when mapping the components, because of
  cycles.

  Example:

  P1 = (x,y) : p, x : A
  Pe = z : A, (z, ?1) : p, ?1 : A, (?1, ?2) :p, ?2 : A

  LHS = [x,y]

  Step 1:

  [
    [[z,?1], [?1,?2]],
    [[z, _],[?1, _],[?2, _]]
  ]

  Within a list such as [[z,1], [1,2]] we have "OR"; that is, either x
  = z and y = 1, or x = 1 and y = 2.
  Between lists, we have "AND".

  We can remove any lists where all bindings are temporary (we don't
  care about subsumptions there) and also lists where all are blank.

  Step 2. Next, we find all combinations of remaining lists.
  Note: This is done stepwise, for two lists at a time to reduce combinations.

  [[z,?1], [z,_]], [[z,?1], [?1,_]], [[z,?1], [?2, _]]

  Step 3. When transposed, we get the possible variables for each LHS variable.

  [[z,z], [?1, _]], [[?z, ?1], [?1, _]], [[z,?2], [?1, _]]

  We turn them into sets and remove blank (they did their job for the
  transpos, keeping variables associated).

  [{z}, {?1}], [{?z, ?1}, {?1}], [{z,?2}, {?1}] => [[z,1]]

  Every list, where all sets have exactly one element, are a mapping
  candidate. Note, that most are mappings. There are some edge
  cases with cycles of the form (x,x) : p, which is why we still
  check the mapping by substituting and subset.
 */
class SubsumptionsFromMappings(
    a: AtomicPatterns,
    shapes: Set[SimpleSHACLShape],
    debug: Boolean = false
)(implicit scopes: Scopes)
    extends Inference:

  /** All combinations of n lists. */
  private def combinationList[T](ls: List[List[T]]): Iterator[List[T]] =
    ls match {
      case Nil => Iterator(Nil)
      case head :: tail =>
        val rec = combinationList[T](tail)
        rec.flatMap(r => head.map(t => t :: r))
    }

  /** Finding associated candidate var in externsion for P1 var. */
  private def represent(
      v: Var,
      a1: AtomicPattern,
      ae: AtomicPattern
  ): Option[Var] =
    import AtomicPattern._
    (a1, ae) match
      case (VAC(v1, c1), VAC(ve, ce)) if v1 == v && c1 == ce       => Some(ve)
      case (VPV(v1, p1, _), VPV(ve, pe, _)) if v1 == v && p1 == pe => Some(ve)
      case (VPL(v1, p1, _), VPV(ve, pe, _)) if v1 == v && p1 == pe => Some(ve)
      case (VPV(_, p1, v1), VPV(_, pe, ve)) if v1 == v && p1 == pe => Some(ve)
      case (LPV(_, p1, v1), VPV(_, pe, ve)) if v1 == v && p1 == pe => Some(ve)
      case _                                                       => None

  /** Combine the constraints of two variable matchings. */
  /*
  Example:

  i1 = [[z,?1]]
  i2 = [[z,_], [?1,_], [?2, _]]

  => [[[z,?1], [z,_]], [[z,?1], [?1,_]], [[z,?1], [?2, _]]] // combinations
  => [[{z}, {?1, _}], [{?z, ?1}, {?1, _}], [{z,?2}, {?1, _}]] // transpose, set
  => [[{z}, {?1, _}], [{?1, _}], [{?1, _}]] // filter 1 or 2 & _
  => [[{z}, {?1, _}]] // filter lhsSize
  => [[{z}, {?1}]] // remove _
  => [[z,?1]] // join elements
   */
  private def reduce(
      i1: List[List[Option[Var]]],
      i2: List[List[Option[Var]]],
      lhsSize: Int
  ): List[List[Option[Var]]] =
    // Get the possible combinations of constraints.
    combinationList(List(i1, i2))
      .map(
        // Transpose them, so we have lists of constraints for one variable.
        ti =>
          ti.transpose
            .map(l => l.toSet)
            // Turn into set. Now, there are four options: // 1. Lists with only one variable (Some(v)) => size 1
            // 2. Lists with only one blank (None) => size 1
            // 3. Lists with one blank (None) and one variable (Some(v))
            //    => size 1 && contains(None)
            // 4. other
            // Cases 1 - 3 are accepted, other rejected.
            .filter(s => s.size == 1 || (s.size == 2 && s.contains(None)))
      )
      // Now, filter each list which has the correct number of sets left.
      .filter(ti => ti.size == lhsSize)
      // Now, remove additional blanks (size == 2), but keep stand-alone blanks.
      .map(ti => ti.map(s => if s.size == 1 then s else s.filter(_.isDefined)))
      // Finally, join elements from now size == 1 sets.
      .map(ti => ti.map(_.head).toList)
      .toList

  /** Construct component map and obtain subsumptions. */
  private def componentMap(
      p1: (Set[Var], Set[AtomicPattern]),
      pext: (Set[Var], Set[AtomicPattern])
  ): Set[Subsumption] =

    // The complete (including temporary) variables in pext and p1.
    val pextv = pext._2.toList.variables
    val p1v = p1._2.toList.variables

    // Determine a fix order of variables.
    val lhs = p1v.toList

    // Construct Step 1, in Algorithm above.
    val t =
      for p1i <- p1._2.toList
      yield (for pexti <- pext._2.toList
      yield lhs.map(represent(_, p1i, pexti)))
        .filter(l =>
          l.exists(o => o.isEmpty || o.map(_.isFresh) == Some(false))
        )
        .filter(l => l.exists(!_.isEmpty))

    // Step by step, apply remaining algorithm above.
    val rhss =
      if t.isEmpty then List()
      else
        t.tail
          .fold(t.head)(reduce(_, _, lhs.size))
          // Finally, we need to filter out all mapping candidate that still contain blanks (None), and then we can remove option.
          .filter(l => !l.exists(_.isEmpty))
          .map(l => l.map(_.toList.head))

    // Construct the mapping, double-check by substituting (for cycles)
    // and finally construct a set of Subsumption axioms as output.
    rhss.flatMap { rhs =>
      val mapping = lhs.zip(rhs).toMap
      // Most mappings are valid, still need to check for cycles.
      if p1._2.toList.mappedWith(mapping).subsumedBy(pext._2.toList) then
        mapping
          // we can filter vacously satisfied subsumptions (i.e., x -> x)
          .filter(_ != _)
          // filter out any temporary variables
          .filter((x, y) => p1._1.contains(x) && pext._1.contains(y))
          // then generate the subsumption axioms (reverse of mapping).
          .map((x, y) => Subsumption(y.asConcept, x.asConcept))
      else Set()
    }.toSet

  def axioms: Set[Axiom] =

    // (1) Detect all components.
    val comps = a.components

    if debug then comps.foreach(println)

    // (2) Construct all extended components.
    val extended = QueryExtensionOptimized(
      comps,
      shapes.map(_.inScope(Scope.Pattern))
    ).extended

    if debug then 
      println("")
      extended.foreach(println)

    // (3) Generate component mappings and find subsumption.
    (for
      // For each sub-pattern component (will be mapped).
      p1 <- comps
      // Test for each extended component, whether there is a mapping.
      pext <- extended
    yield componentMap(p1, pext)).flatten.toSet

    /*

    HashSet(
      VPV(Var(x1),<https://github.com/softlang/s2s/p٭>,Var(y)),
      VAC(Var(y),<https://github.com/softlang/s2s/B٭>),
      B < E => VAC(Var(y),<https://github.com/softlang/s2s/E٭>),

      // WHY?!
      VPV(Var(y),<https://github.com/softlang/s2s/p٭>,Var(?0)),
      VPV(Var(y),<https://github.com/softlang/s2s/p٭>,Var(?4)))

      VAC(Var(?4),<https://github.com/softlang/s2s/B٭>),
      VAC(Var(?0),<https://github.com/softlang/s2s/B٭>),

    */