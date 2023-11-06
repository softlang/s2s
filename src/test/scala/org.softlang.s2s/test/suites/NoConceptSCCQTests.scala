package org.softlang.s2s.test.suites

import org.softlang.s2s.test.ValidationSuite

// Test cases for SCCQ without concept assertions of the form
// (x : A) or (o : A).

class NoConceptSCCQTests extends ValidationSuite:

  val q0 = query("?x :q ?y", "?x :p ?y")

  work("noconcept_0_0", noshapes, q0, noshapes)

  work("noconcept_0_1", Set(":A ⊑ :B"), q0, noshapes)
