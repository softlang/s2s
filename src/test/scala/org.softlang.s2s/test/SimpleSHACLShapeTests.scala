package org.softlang.s2s.test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape

class SimpleSHACLShapeTests extends munit.FunSuite with TestData:

  test("isConceptShape 1") {
    assert(SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isConceptShape)
  }

  test("isConceptShape 2") {
    assert(!SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(NamedRole(p), NamedConcept(C)))).isConceptShape)
  }

  test("isExistsShape 1") {
    assert(!SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isExistsShape)
  }

  test("isExistsShape 2") {
    assert(SimpleSHACLShape(Subsumption(NamedConcept(C), Existential(NamedRole(p), NamedConcept(C)))).isExistsShape)
  }

  test("isForallShape 1") {
    assert(!SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).isForallShape)
  }

  test("isForallShape 2") {
    assert(SimpleSHACLShape(Subsumption(NamedConcept(C), Universal(NamedRole(p), NamedConcept(C)))).isForallShape)
  }

  test("hasExistentialTarget 1") {
    assert(!SimpleSHACLShape(Subsumption(NamedConcept(C), NamedConcept(C))).hasExistentialTarget)
  }

  test("hasExistentialTarget 2") {
    assert(SimpleSHACLShape(Subsumption(Existential(NamedRole(p), NamedConcept(C)), NamedConcept(D))).hasExistentialTarget)
  }
  
  test("validConstraint 1") {
    assert(SimpleSHACLShape.validConstraint(Existential(NamedRole(p), NamedConcept(C))))
  }

  test("validConstraint 2") {
    assert(SimpleSHACLShape.validConstraint(Existential(Inverse(NamedRole(p)), NamedConcept(C))))
  }

  test("validConstraint 3") {
    assert(SimpleSHACLShape.validConstraint(NamedConcept(C)))
  }

  test("validConstraint 4") {
    assert(!SimpleSHACLShape.validConstraint(Union(NamedConcept(C), NamedConcept(D))))
  }

  test("validConstraint 5") {
    assert(!SimpleSHACLShape.validConstraint(Intersection(NamedConcept(C), NamedConcept(D))))
  }

  test("validConstraint 6") {
    assert(!SimpleSHACLShape.validConstraint(Existential(Inverse(NamedRole(p)), Top)))
  }

  test("validConstraint 7") {
    assert(!SimpleSHACLShape.validConstraint(Existential(NamedRole(p), Bottom)))
  }

