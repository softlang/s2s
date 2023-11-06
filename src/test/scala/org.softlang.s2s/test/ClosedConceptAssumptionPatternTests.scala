package org.softlang.s2s.test

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ

class ClosedConceptAssumptionPatternTests extends munit.FunSuite with TestData:

  // The function under test here.
  
  def work(sccq: SCCQ): Set[Axiom] =
    ClosedConceptAssumptionPattern(sccq.pattern).axioms

  //  Empty

  test("query0") {
    val t = work(q0)
    assert(t.isEmpty)
  }

  // Concepts only

  test("query1") {
    val t = work(q1)
    assert(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assert(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assert(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assertEquals(t.size, 3)
  }

  test("query2") {
    val t = work(q2)
    assert(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Dm), x.asConcept)))
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(NamedConcept(C), NamedConcept(D)))))
    assert(t.contains(Subsumption(
      Intersection(NamedConcept(C), NamedConcept(D)), 
      x.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query3") {
    val t = work(q3)
    assert(t.contains(Equality(NamedConcept(Cm), Union(
      x.asConcept, 
      y.asConcept))))
    assert(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assert(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assert(t.contains(Subsumption(y.asConcept, NamedConcept(C))))
    assert(t.contains(Subsumption(NamedConcept(C), y.asConcept)))
    assertEquals(t.size, 5)
  }

  test("query4") {
    val t = work(q4)
    assert(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Dm), y.asConcept)))
    assert(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assert(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assert(t.contains(Subsumption(y.asConcept, NamedConcept(D))))
    assert(t.contains(Subsumption(NamedConcept(D), y.asConcept)))
    assertEquals(t.size, 6)
  }

  // Properties only

  test("query5") {
    val t = work(q5)
    assert(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assert(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept), 
      x.asConcept)))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Existential(Inverse(NamedRole(p)), x.asConcept))))
    assert(t.contains(Subsumption(
      Existential(Inverse(NamedRole(p)), x.asConcept), 
      y.asConcept)))
    assertEquals(t.size, 4)
  }

  test("query6") {
    val t = work(q6)
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(NamedRole(r), y.asConcept))
      )))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(NamedRole(r), y.asConcept)),
      x.asConcept
      )))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(Inverse(NamedRole(r)), x.asConcept))
      )))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(Inverse(NamedRole(r)), x.asConcept)),
      y.asConcept
      )))
    assertEquals(t.size, 4)
  }

  test("query7") {
    val t = work(q7)
    assert(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assert(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept), 
      x.asConcept)))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(r), z.asConcept)),
      y.asConcept
      )))
    assert(t.contains(Subsumption(
      y.asConcept,
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(r), z.asConcept)),
      )))
    assert(t.contains(Subsumption(
      z.asConcept, 
      Existential(Inverse(NamedRole(r)), y.asConcept))))
    assert(t.contains(Subsumption(
      Existential(Inverse(NamedRole(r)), y.asConcept), 
      z.asConcept)))
    assertEquals(t.size, 6)
  }

  // General
  
  test("query8") {
    val t = work(q8)
    assert(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        NamedConcept(C)))))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        NamedConcept(C)),
      x.asConcept)))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Existential(Inverse(NamedRole(p)), x.asConcept))))
    assert(t.contains(Subsumption(
      Existential(Inverse(NamedRole(p)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 5)
  }

  test("query9") {
    val t = work(q9)
    assert(t.contains(Equality(NamedConcept(Cm), y.asConcept)))
    assert(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assert(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept),
      x.asConcept)))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)))))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)),
      y.asConcept)))
    assertEquals(t.size, 5)
  }

  test("query10") {
    val t = work(q10)
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)))))
    assert(t.contains(Subsumption(
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)),
      x.asConcept)))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)))))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)),
      y.asConcept)))
    assert(t.contains(Equality(NamedConcept(Cm), Union(
      x.asConcept, y.asConcept))))
    assertEquals(t.size, 5)
  }

  test("query11") {
    val t = work(q11)
    assert(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Dm), y.asConcept)))
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)))))
    assert(t.contains(Subsumption(
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)),
      x.asConcept)))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(D)))))
    assert(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(D)),
      y.asConcept)))
    assertEquals(t.size, 6)
  }

  // Queries (cyclic)

  test("query12") {
    val t = work(q12)
    assert(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(Inverse(NamedRole(p)), z.asConcept)))))
    assert(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(p), z.asConcept)))))
    assert(t.contains(Subsumption(
      z.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), y.asConcept),
        Existential(NamedRole(p), x.asConcept)))))
    assertEquals(t.size, 3)
  }
