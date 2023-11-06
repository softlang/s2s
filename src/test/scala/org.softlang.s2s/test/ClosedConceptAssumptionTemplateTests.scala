package org.softlang.s2s.test

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ

class ClosedConceptAssumptionTemplateTests extends munit.FunSuite with TestData:

  // The function under test here.
  
  def work(sccq: SCCQ): Set[Axiom] =
    ClosedConceptAssumptionTemplate(sccq.pattern).axioms

  //  Empty

  test("query0") {
    val t = work(q0)
    assert(t.isEmpty)
  }

  // Concepts only

  test("query1") {
    val t = work(q1)
    assert(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertEquals(t.size, 1)
  }

  test("query2") {
    val t = work(q2)
    assert(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Do), x.asConcept)))
    assertEquals(t.size, 2)
  }

  test("query3") {
    val t = work(q3)
    assert(t.contains(Equality(NamedConcept(Co), Union(
      x.asConcept, 
      y.asConcept))))
    assertEquals(t.size, 1)
  }

  test("query4") {
    val t = work(q4)
    assert(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Do), y.asConcept)))
    assertEquals(t.size, 2)
  }

  // Properties only

  test("query5") {
    val t = work(q5)
    assertEquals(t.size, 0)
  }

  test("query6") {
    val t = work(q6)
    assertEquals(t.size, 0)
  }

  test("query7") {
    val t = work(q7)
    assertEquals(t.size, 0)
  }

  // General
  
  test("query8") {
    val t = work(q8)
    assert(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertEquals(t.size, 1)
  }

  test("query9") {
    val t = work(q9)
    assert(t.contains(Equality(NamedConcept(Co), y.asConcept)))
    assertEquals(t.size, 1)
  }

  test("query10") {
    val t = work(q10)
    assert(t.contains(Equality(NamedConcept(Co), Union(
      x.asConcept, y.asConcept))))
    assertEquals(t.size, 1)
  }

  test("query11") {
    val t = work(q11)
    assert(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assert(t.contains(Equality(NamedConcept(Do), y.asConcept)))
    assertEquals(t.size, 2)
  }

  // Queries (cyclic)

  test("query12") {
    val t = work(q12)
    assertEquals(t.size, 0)
  }
