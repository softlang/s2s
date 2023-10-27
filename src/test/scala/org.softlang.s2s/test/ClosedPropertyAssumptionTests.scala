package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.rules.TestName
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName

import de.pseifer.shar.dl._
import org.softlang.s2s.infer._
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.core.Scope

class ClosedPropertyAssumptionTests extends TestData:

  @Rule
  def name = _name
  val _name = TestName()

  // The function under test here.
  
  def workH(sccq: SCCQ): Set[Axiom] =
    ClosedPropertyAssumption(sccq.template, Scope.Out).axioms

  def workP(sccq: SCCQ): Set[Axiom] =
    ClosedPropertyAssumption(sccq.pattern, Scope.Med).axioms

  //  Empty

  @Test def query0P(): Unit =
    val t = workP(q0)
    assertTrue(t.isEmpty)

  @Test def query0H(): Unit =
    val t = workH(q0)
    assertTrue(t.isEmpty)

  // Concepts only

  @Test def query1P(): Unit =
    val t = workP(q1)
    assertEquals(t.size, 0)

  @Test def query1H(): Unit =
    val t = workH(q1)
    assertEquals(t.size, 0)

  @Test def query2P(): Unit =
    val t = workP(q2)
    assertEquals(t.size, 0)

  @Test def query2H(): Unit =
    val t = workH(q2)
    assertEquals(t.size, 0)

  @Test def query3P(): Unit =
    val t = workP(q3)
    assertEquals(t.size, 0)

  @Test def query3H(): Unit =
    val t = workH(q3)
    assertEquals(t.size, 0)
    
  @Test def query4P(): Unit =
    val t = workP(q4)
    assertEquals(t.size, 0)

  @Test def query4H(): Unit =
    val t = workH(q4)
    assertEquals(t.size, 0)

  // Properties only

  @Test def query5P(): Unit =
    val t = workP(q5)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query5H(): Unit =
    val t = workH(q5)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query6P(): Unit =
    val t = workP(q6)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(rm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(rm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(rm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(rm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 8)

  @Test def query6H(): Unit =
    val t = workH(q6)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(ro), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(ro), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(ro)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(ro), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 8)

  @Test def query7P(): Unit =
    val t = workP(q7)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(rm), Top),
      Intersection(
        y.asConcept, 
        Existential(NamedRole(rm), z.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), Top),
      Intersection(
        z.asConcept, 
        Existential(Inverse(NamedRole(rm)), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(rm), z.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(rm)), y.asConcept),
      z.asConcept)))
    assertEquals(t.size, 8)

  @Test def query7H(): Unit =
    val t = workH(q7)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(ro), Top),
      Intersection(
        y.asConcept, 
        Existential(NamedRole(ro), z.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), Top),
      Intersection(
        z.asConcept, 
        Existential(Inverse(NamedRole(ro)), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(ro), z.asConcept),
      y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(ro)), y.asConcept),
      z.asConcept)))
    assertEquals(t.size, 8)

  // General
  
  @Test def query8P(): Unit =
    val t = workP(q8)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query8H(): Unit =
    val t = workH(q8)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query9P(): Unit =
    val t = workP(q9)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query9H(): Unit =
    val t = workH(q9)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query10P(): Unit =
    val t = workP(q10)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query10H(): Unit =
    val t = workH(q10)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query11P(): Unit =
    val t = workP(q11)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(pm), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(pm)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)

  @Test def query11H(): Unit =
    val t = workH(q11)
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), Top),
      Intersection(
        x.asConcept, 
        Existential(NamedRole(po), y.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), Top),
      Intersection(
        y.asConcept, 
        Existential(Inverse(NamedRole(po)), x.asConcept)))))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept),
      x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept),
      y.asConcept)))
    assertEquals(t.size, 4)
    
  // Queries (cyclic)

  @Test def query12P(): Unit =
    val t = workP(q12)
    assertTrue(t.contains(Equality(
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
    assertTrue(t.contains(Equality(
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
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), x.asConcept), z.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), y.asConcept), x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(pm), z.asConcept), y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), x.asConcept), y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), y.asConcept), z.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(pm)), z.asConcept), x.asConcept)))
    assertEquals(t.size, 8)

  @Test def query12H(): Unit =
    val t = workH(q12)
    assertTrue(t.contains(Equality(
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
    assertTrue(t.contains(Equality(
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
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), x.asConcept), z.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), y.asConcept), x.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(NamedRole(po), z.asConcept), y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), x.asConcept), y.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), y.asConcept), z.asConcept)))
    assertTrue(t.contains(Equality(
      Existential(Inverse(NamedRole(po)), z.asConcept), x.asConcept)))
    assertEquals(t.size, 8)
