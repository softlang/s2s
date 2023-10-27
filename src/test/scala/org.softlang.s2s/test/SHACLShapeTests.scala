package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape

class SHACLShapeTests extends TestData:
  
  @Test def testValidTarget1(): Unit =
    assertTrue(SHACLShape.validTarget(NamedConcept(C)))

  @Test def testValidTarget2(): Unit =
    assertTrue(SHACLShape.validTarget(Existential(NamedRole(p), Top)))

  @Test def testValidTarget3(): Unit =
    assertTrue(SHACLShape.validTarget(Existential(Inverse(NamedRole(p)), Top)))

  @Test def testValidTarget4(): Unit =
    assertFalse(SHACLShape.validTarget(Union(NamedConcept(C), NamedConcept(D))))

  @Test def testValidTarget5(): Unit =
    assertFalse(SHACLShape.validTarget(Existential(NamedRole(p), NamedConcept(C))))

  @Test def testValidConstraint1(): Unit =
    // Note: Any concept is valid here.
    assertTrue(SHACLShape.validConstraint(Existential(NamedRole(p), NamedConcept(C))))

  @Test def testToSimple1(): Unit =
    assertTrue(s1.toSimple.isDefined)

  @Test def testToSimple2(): Unit =
    assertTrue(s2.toSimple.isEmpty)

