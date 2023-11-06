package org.softlang.s2s.test

import de.pseifer.shar.dl._

import org.softlang.s2s.core.Scope
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.SimpleSHACLShape

class SHACLShapeTests extends munit.FunSuite with TestData:
  
  test("validTarget 1") {
    assert(SHACLShape.validTarget(NamedConcept(C)))
  }

  test("validTarget 2") {
    assert(SHACLShape.validTarget(Existential(NamedRole(p), Top)))
  }

  test("validTarget 3") {
    assert(SHACLShape.validTarget(Existential(Inverse(NamedRole(p)), Top)))
  }

  test("validTarget 4") {
    assert(!SHACLShape.validTarget(Union(NamedConcept(C), NamedConcept(D))))
  }

  test("validTarget 5") {
    assert(!SHACLShape.validTarget(Existential(NamedRole(p), NamedConcept(C))))
  }

  test("validConstraint 1") {
    // Note: Any concept is valid here.
    assert(SHACLShape.validConstraint(Existential(NamedRole(p), NamedConcept(C))))
  }

  test("toSimple 1") {
    assert(s1.toSimple.isDefined)
  }

  test("toSimple 2") {
    assert(s2.toSimple.isEmpty)
  }

