package org.softlang.s2s.test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class AtomicPatternTests extends munit.FunSuite with TestData:

  test("Properties (empty)") {
    assertEquals(q1.pattern.head.properties, Set())
  }

  test("Properties (non-empty)") {
    assertEquals(q5.pattern.head.properties, Set(NamedRole(p)))
  }
  
  test("Concepts (non-empty)") {
    assertEquals(q1.pattern.head.concepts, Set(NamedConcept(C)))
  }

  test("Concepts (empty)") {
    assertEquals(q5.pattern.head.concepts, Set())
  }

  test("Variable (single)") {
    assertEquals(q1.pattern.head.variables, Set(Var("x")))
    assertEquals(q1.pattern.head.variables, Set(x))
  }

  test("Variable (multiple)") {
    assertEquals(q5.pattern.head.variables, Set(Var("x"), Var("y")))
    assertEquals(q5.pattern.head.variables, Set(x, y))
  }

