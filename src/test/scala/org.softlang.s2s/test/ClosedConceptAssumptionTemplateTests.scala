package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.rules.TestName
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ

class ClosedConceptAssumptionTemplateTests extends TestData:

  @Rule
  def name = _name
  val _name = TestName()

  // The function under test here.
  
  def work(sccq: SCCQ): Set[Axiom] =
    ClosedConceptAssumptionTemplate(sccq.pattern).axioms

  //  Empty

  @Test def query0(): Unit =
    val t = work(q0)
    assertTrue(t.isEmpty)

  // Concepts only

  @Test def query1(): Unit =
    val t = work(q1)
    assertTrue(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertEquals(t.size, 1)

  @Test def query2(): Unit =
    val t = work(q2)
    assertTrue(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Do), x.asConcept)))
    assertEquals(t.size, 2)

  @Test def query3(): Unit =
    val t = work(q3)
    assertTrue(t.contains(Equality(NamedConcept(Co), Union(
      x.asConcept, 
      y.asConcept))))
    assertEquals(t.size, 1)

  @Test def query4(): Unit =
    val t = work(q4)
    assertTrue(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Do), y.asConcept)))
    assertEquals(t.size, 2)

  // Properties only

  @Test def query5(): Unit =
    val t = work(q5)
    assertEquals(t.size, 0)

  @Test def query6(): Unit =
    val t = work(q6)
    assertEquals(t.size, 0)

  @Test def query7(): Unit =
    val t = work(q7)
    assertEquals(t.size, 0)

  // General
  
  @Test def query8(): Unit =
    val t = work(q8)
    assertTrue(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertEquals(t.size, 1)

  @Test def query9(): Unit =
    val t = work(q9)
    assertTrue(t.contains(Equality(NamedConcept(Co), y.asConcept)))
    assertEquals(t.size, 1)

  @Test def query10(): Unit =
    val t = work(q10)
    assertTrue(t.contains(Equality(NamedConcept(Co), Union(
      x.asConcept, y.asConcept))))
    assertEquals(t.size, 1)

  @Test def query11(): Unit =
    val t = work(q11)
    assertTrue(t.contains(Equality(NamedConcept(Co), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Do), y.asConcept)))
    assertEquals(t.size, 2)

  // Queries (cyclic)

  @Test def query12(): Unit =
    val t = work(q12)
    assertEquals(t.size, 0)
