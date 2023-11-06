package org.softlang.s2s.test

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.core.Scope

class ClosedPropertyAssumptionTests extends munit.FunSuite with TestData:

  // The function under test here.
  
  def workH(sccq: SCCQ): Set[Axiom] =
    ClosedPropertyAssumption(sccq.template, Scope.Out).axioms

  def workP(sccq: SCCQ): Set[Axiom] =
    ClosedPropertyAssumption(sccq.pattern, Scope.Med).axioms

  //  Empty

  test("query0P") {
    val t = workP(q0)
    assert(t.isEmpty)
  }

  test("query0H") {
    val t = workH(q0)
    assert(t.isEmpty)
  }

  // Concepts only

  test("query1P") {
    val t = workP(q1)
    assertEquals(t.size, 0)
  }

  test("query1H") {
    val t = workH(q1)
    assertEquals(t.size, 0)
  }

  test("query2P") {
    val t = workP(q2)
    assertEquals(t.size, 0)
  }

  test("query2H") {
    val t = workH(q2)
    assertEquals(t.size, 0)
  }

  test("query3P") {
    val t = workP(q3)
    assertEquals(t.size, 0)
  }

  test("query3H") {
    val t = workH(q3)
    assertEquals(t.size, 0)
    
  test("query4P") {
    val t = workP(q4)
    assertEquals(t.size, 0)
  }
  }

  test("query4H") {
    val t = workH(q4)
    assertEquals(t.size, 0)
  }

  // Properties only

  test("query5P") {
    val t = workP(q5)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query5H") {
    val t = workH(q5)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query6P") {
    val t = workP(q6)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(rm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(rm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(rm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(rm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 8)
  }

  test("query6H") {
    val t = workH(q6)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(ro), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(ro), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(ro)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(ro), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 8)
  }

  test("query7P") {
    val t = workP(q7)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(rm), Top),
      Intersection(
        y.asConcept, 
        Existential(NamedRole(rm), z.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), Top),
      Intersection(
        z.asConcept, 
        Existential(Inverse(NamedRole(rm)), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(rm), z.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), y.asConcept),
      z.asConcept)))
    assertEquals(t.size, 8)
  }

  test("query7H") {
    val t = workH(q7)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(ro), Top),
      Intersection(
        y.asConcept, 
        Existential(NamedRole(ro), z.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), Top),
      Intersection(
        z.asConcept, 
        Existential(Inverse(NamedRole(ro)), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(ro), z.asConcept),
      y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), y.asConcept),
      z.asConcept)))
    assertEquals(t.size, 8)
  }

  // General
  
  test("query8P") {
    val t = workP(q8)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query8H") {
    val t = workH(q8)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query9P") {
    val t = workP(q9)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query9H") {
    val t = workH(q9)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query10P") {
    val t = workP(q10)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query10H") {
    val t = workH(q10)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query11P") {
    val t = workP(q11)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query11H") {
    val t = workH(q11)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
    
  // Queries (cyclic)
  }

  test("query12P") {
    val t = workP(q12)
    assert(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Union(
        Union(
        Intersection(
          x.asConcept, 
          Existential(NamedRole(pm), y.asConcept)),
        Intersection(
            y.asConcept, 
            Existential(NamedRole(pm), z.asConcept))),
          Intersection(
            z.asConcept,
            Existential(NamedRole(pm), x.asConcept))))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Union(
        Union(
        Intersection(
          y.asConcept, 
          Existential(Inverse(NamedRole(pm)), x.asConcept)),
        Intersection(
            z.asConcept, 
            Existential(Inverse(NamedRole(pm)), y.asConcept))),
          Intersection(
            x.asConcept,
            Existential(Inverse(NamedRole(pm)), z.asConcept))))))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), x.asConcept), z.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept), x.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(pm), z.asConcept), y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept), y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), y.asConcept), z.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), z.asConcept), x.asConcept)))
    assertEquals(t.size, 8)
  }

  test("query12H") {
    val t = workH(q12)
    assert(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Union(
        Union(
        Intersection(
          x.asConcept, 
          Existential(NamedRole(po), y.asConcept)),
        Intersection(
            y.asConcept, 
            Existential(NamedRole(po), z.asConcept))),
          Intersection(
            z.asConcept,
            Existential(NamedRole(po), x.asConcept))))))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Union(
        Union(
        Intersection(
          y.asConcept, 
          Existential(Inverse(NamedRole(po)), x.asConcept)),
        Intersection(
            z.asConcept, 
            Existential(Inverse(NamedRole(po)), y.asConcept))),
          Intersection(
            x.asConcept,
            Existential(Inverse(NamedRole(po)), z.asConcept))))))
    assert(t.contains(Equality(
      Existential(NamedRole(po), x.asConcept), z.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept), x.asConcept)))
    assert(t.contains(Equality(
      Existential(NamedRole(po), z.asConcept), y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept), y.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), y.asConcept), z.asConcept)))
    assert(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), z.asConcept), x.asConcept)))
    assertEquals(t.size, 8)
  }

