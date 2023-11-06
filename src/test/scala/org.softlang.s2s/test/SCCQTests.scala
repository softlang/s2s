package org.softlang.s2s.test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class SCCQTests extends munit.FunSuite with TestData:

  // Properties, concepts and variables.

  test("Properties (empty)") {
    assertEquals(q1.properties, Set())
  }

  test("Properties (non-empty)") {
    assertEquals(q5.properties, Set(NamedRole(p)))
  }
  
  test("Concepts (non-empty)") {
    assertEquals(q1.concepts, Set(NamedConcept(C)))
  }

  test("Concepts (empty)") {
    assertEquals(q5.concepts, Set())
  }

  test("Variables (single)") {
    assertEquals(q1.variables, Set(Var("x")))
    assertEquals(q1.pattern.head.variables, Set(x))
  }

  test("Variables (multiple)") {
    assertEquals(q5.variables, Set(Var("x"), Var("y")))
    assertEquals(q5.variables, Set(x, y))
  }

  // Cycles

  test("Cyclic 1") {
    assert(!q1.pattern.hasCyclicVCG)
    assert(!q2.pattern.hasCyclicVCG)
    assert(!q3.pattern.hasCyclicVCG)
    assert(!q4.pattern.hasCyclicVCG)
    assert(!q5.pattern.hasCyclicVCG)
    assert(!q6.pattern.hasCyclicVCG)
    assert(!q7.pattern.hasCyclicVCG)
    assert(!q8.pattern.hasCyclicVCG)
    assert(!q9.pattern.hasCyclicVCG)
    assert(!q10.pattern.hasCyclicVCG)
    assert(!q11.pattern.hasCyclicVCG)
  }

  test("Cyclic 2") {
    assert(!q1.template.hasCyclicVCG)
    assert(!q2.template.hasCyclicVCG)
    assert(!q3.template.hasCyclicVCG)
    assert(!q4.template.hasCyclicVCG)
    assert(!q5.template.hasCyclicVCG)
    assert(!q6.template.hasCyclicVCG)
    assert(!q7.template.hasCyclicVCG)
    assert(!q8.template.hasCyclicVCG)
    assert(!q9.template.hasCyclicVCG)
    assert(!q10.template.hasCyclicVCG)
    assert(!q11.template.hasCyclicVCG)
  }

  test("Cyclic 3") {
    assert(q12.pattern.hasCyclicVCG)
  }

  test("Cyclic 4") {
    assert(q12.template.hasCyclicVCG)
  }

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

  test("Cyclic 5") {
    assert(q13.pattern.hasCyclicVCG)
  }

  test("Cyclic 6") {
    assert(q13.template.hasCyclicVCG)
  }

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

  test("Cyclic 7") {
    assert(q14.pattern.hasCyclicVCG)
  }

  test("Cyclic 8") {
    assert(q14.template.hasCyclicVCG)
  }

  // Components
  
  test("Components 1") {
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
  }

  test("Components 2") {
    assertEquals(q3.pattern.components.size, 2)
    assertEquals(q4.pattern.components.size, 2)
  }

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

  test("Components 3") {
    assertEquals(q15.pattern.components.size, 3)
    assertEquals(q15.template.components.size, 3)
  }

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

  test("Components 4") {
    assertEquals(q16.pattern.components.size, 3)
    assertEquals(q16.template.components.size, 3)
  }

