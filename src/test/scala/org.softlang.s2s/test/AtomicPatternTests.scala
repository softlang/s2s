package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Var
import org.softlang.s2s.query._

class AtomicPatternTests extends TestData:

  @Test def testProperties1(): Unit =
    assertEquals(q1.pattern.head.properties, Set())

  @Test def testProperties2(): Unit =
    assertEquals(q5.pattern.head.properties, Set(NamedRole(p)))
  
  @Test def testConcepts1(): Unit =
    assertEquals(q1.pattern.head.concepts, Set(NamedConcept(C)))

  @Test def testConcepts2(): Unit =
    assertEquals(q5.pattern.head.concepts, Set())

  @Test def testVariable1(): Unit =
    assertEquals(q1.pattern.head.variables, Set(Var("x")))
    assertEquals(q1.pattern.head.variables, Set(x))

  @Test def testVariable2(): Unit =
    assertEquals(q5.pattern.head.variables, Set(Var("x"), Var("y")))
    assertEquals(q5.pattern.head.variables, Set(x, y))

