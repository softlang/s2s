package org.softlang.s2s.test

import org.junit.Assert.*
import org.junit.Test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape

class SimpleSHACLShapeTests extends TestData:

  @Test def testIsConceptShape1(): Unit =
    assertTrue(SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isConceptShape)

  @Test def testIsConceptShape2(): Unit =
    assertFalse(SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(NamedRole(p), NamedConcept(C)))).isConceptShape)

  @Test def testIsExistsShape1(): Unit =
    assertFalse(SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isExistsShape)

  @Test def testIsExistsShape2(): Unit =
    assertTrue(SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(NamedRole(p), NamedConcept(C)))).isExistsShape)

  @Test def testIsForallShape1(): Unit =
    assertFalse(SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isForallShape)

  @Test def testIsForallShape2(): Unit =
    assertTrue(SimpleSHACLShape(Subsumption(NamedConcept(C), Universal(NamedRole(p), NamedConcept(C)))).isForallShape)

  @Test def testhasExistentialTarget1(): Unit =
    assertFalse(SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).hasExistentialTarget)

  @Test def testhasExistentialTarget2(): Unit =
    assertTrue(SimpleSHACLShape(Subsumption(Existential(NamedRole(p), NamedConcept(C)), NamedConcept(D))).hasExistentialTarget)
  
  @Test def testValidConstraint1(): Unit =
    assertTrue(SimpleSHACLShape.validConstraint(Existential(NamedRole(p), NamedConcept(C))))

  @Test def testValidConstraint2(): Unit =
    assertTrue(SimpleSHACLShape.validConstraint(Existential(Inverse(NamedRole(p)), NamedConcept(C))))

  @Test def testValidConstraint3(): Unit =
    assertTrue(SimpleSHACLShape.validConstraint(NamedConcept(C)))

  @Test def testValidConstraint4(): Unit =
    assertFalse(SimpleSHACLShape.validConstraint(Union(NamedConcept(C), NamedConcept(D))))

  @Test def testValidConstraint5(): Unit =
    assertFalse(SimpleSHACLShape.validConstraint(Intersection(NamedConcept(C), NamedConcept(D))))

  @Test def testValidConstraint6(): Unit =
    assertFalse(SimpleSHACLShape.validConstraint(Existential(Inverse(NamedRole(p)), Top)))

  @Test def testValidConstraint7(): Unit =
    assertFalse(SimpleSHACLShape.validConstraint(Existential(NamedRole(p), Bottom)))

