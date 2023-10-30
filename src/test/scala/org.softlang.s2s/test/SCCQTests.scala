package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class SCCQTests extends TestData:

  // Properties, concepts and variables.

  @Test def testProperties1(): Unit =
    assertEquals(q1.properties, Set())

  @Test def testProperties2(): Unit =
    assertEquals(q5.properties, Set(NamedRole(p)))
  
  @Test def testConcepts1(): Unit =
    assertEquals(q1.concepts, Set(NamedConcept(C)))

  @Test def testConcepts2(): Unit =
    assertEquals(q5.concepts, Set())

  @Test def testVariable1(): Unit =
    assertEquals(q1.variables, Set(Var("x")))
    assertEquals(q1.pattern.head.variables, Set(x))

  @Test def testVariable2(): Unit =
    assertEquals(q5.variables, Set(Var("x"), Var("y")))
    assertEquals(q5.variables, Set(x, y))

  // Cycles

  @Test def testCyclic1(): Unit =
    assertFalse(q1.pattern.hasCyclicVCG)
    assertFalse(q2.pattern.hasCyclicVCG)
    assertFalse(q3.pattern.hasCyclicVCG)
    assertFalse(q4.pattern.hasCyclicVCG)
    assertFalse(q5.pattern.hasCyclicVCG)
    assertFalse(q6.pattern.hasCyclicVCG)
    assertFalse(q7.pattern.hasCyclicVCG)
    assertFalse(q8.pattern.hasCyclicVCG)
    assertFalse(q9.pattern.hasCyclicVCG)
    assertFalse(q10.pattern.hasCyclicVCG)
    assertFalse(q11.pattern.hasCyclicVCG)

  @Test def testCyclic2(): Unit =
    assertFalse(q1.template.hasCyclicVCG)
    assertFalse(q2.template.hasCyclicVCG)
    assertFalse(q3.template.hasCyclicVCG)
    assertFalse(q4.template.hasCyclicVCG)
    assertFalse(q5.template.hasCyclicVCG)
    assertFalse(q6.template.hasCyclicVCG)
    assertFalse(q7.template.hasCyclicVCG)
    assertFalse(q8.template.hasCyclicVCG)
    assertFalse(q9.template.hasCyclicVCG)
    assertFalse(q10.template.hasCyclicVCG)
    assertFalse(q11.template.hasCyclicVCG)

  @Test def testCyclic3(): Unit =
    assertTrue(q12.pattern.hasCyclicVCG)

  @Test def testCyclic4(): Unit =
    assertTrue(q12.template.hasCyclicVCG)

  val q13 = SCCQ(
    List(
      AtomicPattern.VPV(w, p, x),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(z, p, w),
    ), 
    List(
      AtomicPattern.VPV(w, p, x),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(z, p, y),
    ))

  @Test def testCyclic5(): Unit =
    assertTrue(q13.pattern.hasCyclicVCG)

  @Test def testCyclic6(): Unit =
    assertTrue(q13.template.hasCyclicVCG)

  val q14 = SCCQ(
    List(
      AtomicPattern.VPV(w, p, x),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(y, p, z),
    ), 
    List(
      AtomicPattern.VPV(w, p, x),
      AtomicPattern.VPV(x, p, y),
      AtomicPattern.VPV(y, p, z),
      AtomicPattern.VPV(z, p, y),
    ))

  @Test def testCyclic7(): Unit =
    assertTrue(q14.pattern.hasCyclicVCG)

  @Test def testCyclic8(): Unit =
    assertTrue(q14.template.hasCyclicVCG)

  // Components
  
  @Test def testComponents1(): Unit =
    assertEquals(q1.pattern.components.size, 1)
    assertEquals(q2.pattern.components.size, 1)
    assertEquals(q5.pattern.components.size, 1)
    assertEquals(q6.pattern.components.size, 1)
    assertEquals(q7.pattern.components.size, 1)
    assertEquals(q8.pattern.components.size, 1)
    assertEquals(q9.pattern.components.size, 1)
    assertEquals(q10.pattern.components.size, 1)
    assertEquals(q11.pattern.components.size, 1)
    assertEquals(q12.pattern.components.size, 1)
    assertEquals(q13.pattern.components.size, 1)
    assertEquals(q14.pattern.components.size, 1)
    assertEquals(q13.template.components.size, 1)
    assertEquals(q14.template.components.size, 1)

  @Test def testComponents2(): Unit =
    assertEquals(q3.pattern.components.size, 2)
    assertEquals(q4.pattern.components.size, 2)

  val q15 = SCCQ(
    List(
      AtomicPattern.VPV(x1, p, y1),
      AtomicPattern.VPV(x2, p, y2),
      AtomicPattern.VPV(x3, r, y3)
    ), 
    List(
      AtomicPattern.VPV(x1, p, y1),
      AtomicPattern.VPV(x2, p, y2),
      AtomicPattern.VAC(x3, C)
    ))

  @Test def testComponents3(): Unit =
    assertEquals(q15.pattern.components.size, 3)
    assertEquals(q15.template.components.size, 3)

  val q16 = SCCQ(
    List(
      AtomicPattern.VAC(x1, C),
      AtomicPattern.VPV(x2, p, y2),
      AtomicPattern.VAC(x3, C)
    ), 
    List(
      AtomicPattern.VAC(x1, C),
      AtomicPattern.VAC(x2, D),
      AtomicPattern.VAC(x3, C)
    ))

  @Test def testComponents4(): Unit =
    assertEquals(q16.pattern.components.size, 3)
    assertEquals(q16.template.components.size, 3)
