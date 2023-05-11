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
  
  [[z,?1], [z,_]], [[z,?1], [?1,_]], [[z,?1], [?2, _]]

  Step 3. When transposed, we get the possible variables for each LHS variable.

  [[z,z], [?1, _]], [[?z, ?1], [?1, _]], [[z,?2], [?1, _]]

  We turn them into sets and remove blank (they did their job for the
  transpos, keeping variables associated).

  [{z}, {?1}], [{?z, ?1}, {?1}], [{z,?2}, {?1}]

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
  private def combinationList[T](ls:List[List[T]]): Iterator[List[T]] = 
    ls match {
      case Nil => Iterator(Nil)
      case head :: tail => val rec = combinationList[T](tail)
                            rec.flatMap(r => head.map(t => t::r))
    }

  /** Finding associated candidate var in externsion for P1 var. */
  private def represent(v: Var, a1: AtomicPattern, ae: AtomicPattern)
      : Option[Var] = 
    import AtomicPattern._
    (a1, ae) match
      case (VAC(v1, c1), VAC(ve, ce)) if v1 == v && c1 == ce => Some(ve)
      case (VPV(v1, p1, _), VPV(ve, pe, _)) if v1 == v && p1 == pe => Some(ve)
      case (VPL(v1, p1, _), VPV(ve, pe, _)) if v1 == v && p1 == pe => Some(ve)
      case (VPV(_, p1, v1), VPV(_, pe, ve)) if v1 == v && p1 == pe => Some(ve)
      case (LPV(_, p1, v1), VPV(_, pe, ve)) if v1 == v && p1 == pe => Some(ve)
      case _ => None

  /** Construct component map and obtain subsumptions. */
  private def componentMap(
    p1: (Set[Var], Set[AtomicPattern]), 
    pext: (Set[Var], Set[AtomicPattern]))
      : Set[Subsumption] = 
    // The complete (including temporary) variables in pext and p1.
    val pextv = pext._2.toList.variables
    val p1v = p1._2.toList.variables

    // Determine a fix order of variables.
    val lhs = p1v.toList

    // Construct Step 1, in Algorithm above.
    val t = 
      for p1i <- p1._2.toList yield
        (for pexti <- pext._2.toList yield
          lhs.map(represent(_, p1i, pexti)))
            .filter(l => 
              l.exists(o => o.isEmpty || o.map(_.isFresh) == Some(false)))
            .filter(l =>
              l.exists(!_.isEmpty))

    // Remaining steps of the Algorithm above.
    val rhss = 
      combinationList(t).map(ti => 
        ti.transpose.map(l => 
          l.toSet.filter(_.isDefined)).filter(_.size == 1))
          .filter(ti => 
            ti.size == lhs.size)
          .map(ti => ti.map(_.head.toList.head))

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

    // (3) Generate component mappings and find subsumption.
    (for
      // The extended component.
      pext <- extendedComps
      // The sub-pattern component (will be mapped).
      p1 <- comps
    yield componentMap(p1, pext)).flatten.toSet
