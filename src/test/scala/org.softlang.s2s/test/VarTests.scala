package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import org.softlang.s2s.core.Var

class VarTests extends TestData:
  
  @Test def testFreshVariable1(): Unit =
    assertTrue(Var.fresh().isFresh)
    assertTrue(Var.fresh().isFresh)

  @Test def testFreshVariable2(): Unit =
    assertFalse(w.isFresh)
    assertFalse(x.isFresh)
    assertFalse(y.isFresh)
    assertFalse(z.isFresh)

  @Test def testAsConcept1(): Unit =
    val xStr = "<https://github.com/pseifer/shar/ontology/x•-1>"
    assertEquals(x.asConcept.toString, xStr)
