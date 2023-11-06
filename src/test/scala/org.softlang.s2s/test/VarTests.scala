package org.softlang.s2s.test

import org.softlang.s2s.core.Var

class VarTests extends munit.FunSuite with TestData:
  
  test("testFreshVariable1") {
    assert(Var.fresh().isFresh)
    assert(Var.fresh().isFresh)
  }

  test("testFreshVariable2") {
    assert(!w.isFresh)
    assert(!x.isFresh)
    assert(!y.isFresh)
    assert(!z.isFresh)
  }

  test("testAsConcept1") {
    val xStr = "<https://github.com/pseifer/shar/ontology/x•-1>"
    assertEquals(x.asConcept.toString, xStr)
  }
