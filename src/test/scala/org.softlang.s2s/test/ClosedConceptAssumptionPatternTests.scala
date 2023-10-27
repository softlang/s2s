package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.rules.TestName
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ

class ClosedConceptAssumptionPatternTests extends TestData:

  @Rule
  def name = _name
  val _name = TestName()

  // The function under test here.
  
  def work(sccq: SCCQ): Set[Axiom] =
    ClosedConceptAssumptionPattern(sccq.pattern).axioms

  //  Empty

  @Test def query0(): Unit =
    val t = work(q0)
    assertTrue(t.isEmpty)

  // Concepts only

  @Test def query1(): Unit =
    val t = work(q1)
    assertTrue(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assertTrue(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assertTrue(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assertEquals(t.size, 3)

  @Test def query2(): Unit =
    val t = work(q2)
    assertTrue(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Dm), x.asConcept)))
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(NamedConcept(C), NamedConcept(D)))))
    assertTrue(t.contains(Subsumption(
      Intersection(NamedConcept(C), NamedConcept(D)), 
      x.asConcept)))
    assertEquals(t.size, 4)

  @Test def query3(): Unit =
    val t = work(q3)
    assertTrue(t.contains(Equality(NamedConcept(Cm), Union(
      x.asConcept, 
      y.asConcept))))
    assertTrue(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assertTrue(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assertTrue(t.contains(Subsumption(y.asConcept, NamedConcept(C))))
    assertTrue(t.contains(Subsumption(NamedConcept(C), y.asConcept)))
    assertEquals(t.size, 5)

  @Test def query4(): Unit =
    val t = work(q4)
    assertTrue(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Dm), y.asConcept)))
    assertTrue(t.contains(Subsumption(x.asConcept, NamedConcept(C))))
    assertTrue(t.contains(Subsumption(NamedConcept(C), x.asConcept)))
    assertTrue(t.contains(Subsumption(y.asConcept, NamedConcept(D))))
    assertTrue(t.contains(Subsumption(NamedConcept(D), y.asConcept)))
    assertEquals(t.size, 6)

  // Properties only

  @Test def query5(): Unit =
    val t = work(q5)
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept), 
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Existential(Inverse(NamedRole(p)), x.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(Inverse(NamedRole(p)), x.asConcept), 
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query6(): Unit =
    val t = work(q6)
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(NamedRole(r), y.asConcept))
      )))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(NamedRole(r), y.asConcept)),
      x.asConcept
      )))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(Inverse(NamedRole(r)), x.asConcept))
      )))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(Inverse(NamedRole(r)), x.asConcept)),
      y.asConcept
      )))
    assertEquals(t.size, 4)

  @Test def query7(): Unit =
    val t = work(q7)
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept), 
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(r), z.asConcept)),
      y.asConcept
      )))
    assertTrue(t.contains(Subsumption(
      y.asConcept,
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(r), z.asConcept)),
      )))
    assertTrue(t.contains(Subsumption(
      z.asConcept, 
      Existential(Inverse(NamedRole(r)), y.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(Inverse(NamedRole(r)), y.asConcept), 
      z.asConcept)))
    assertEquals(t.size, 6)

  // General
  
  @Test def query8(): Unit =
    val t = work(q8)
    assertTrue(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        NamedConcept(C)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        NamedConcept(C)),
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Existential(Inverse(NamedRole(p)), x.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(Inverse(NamedRole(p)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 5)

  @Test def query9(): Unit =
    val t = work(q9)
    assertTrue(t.contains(Equality(NamedConcept(Cm), y.asConcept)))
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Existential(NamedRole(p), y.asConcept))))
    assertTrue(t.contains(Subsumption(
      Existential(NamedRole(p), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)),
      y.asConcept)))
    assertEquals(t.size, 5)

  @Test def query10(): Unit =
    val t = work(q10)
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)),
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(C)),
      y.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Cm), Union(
      x.asConcept, y.asConcept))))
    assertEquals(t.size, 5)

  @Test def query11(): Unit =
    val t = work(q11)
    assertTrue(t.contains(Equality(NamedConcept(Cm), x.asConcept)))
    assertTrue(t.contains(Equality(NamedConcept(Dm), y.asConcept)))
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        NamedConcept(C),
        Existential(NamedRole(p), y.asConcept)),
      x.asConcept)))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(D)))))
    assertTrue(t.contains(Subsumption(
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        NamedConcept(D)),
      y.asConcept)))
    assertEquals(t.size, 6)

  // Queries (cyclic)

  @Test def query12(): Unit =
    val t = work(q12)
    assertTrue(t.contains(Subsumption(
      x.asConcept, 
      Intersection(
        Existential(NamedRole(p), y.asConcept),
        Existential(Inverse(NamedRole(p)), z.asConcept)))))
    assertTrue(t.contains(Subsumption(
      y.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), x.asConcept),
        Existential(NamedRole(p), z.asConcept)))))
    assertTrue(t.contains(Subsumption(
      z.asConcept, 
      Intersection(
        Existential(Inverse(NamedRole(p)), y.asConcept),
        Existential(NamedRole(p), x.asConcept)))))
    assertEquals(t.size, 3)
