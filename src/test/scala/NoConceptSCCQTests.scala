package org.softlang.s2s.test

import org.junit.Test
import org.junit.Rule
import org.junit.rules.TestName

// Test cases for SCCQ without concept assertions of the form
// (x : A) or (o : A).

class NoConceptSCCQTests extends ValidationTests("No-Concept"):

  @Rule
  def name = _name
  val _name = TestName()

  val q0 = query("?x :q ?y", "?x :p ?y")

  @Test def noconcept_0_0(): Unit =
    test(noshapes, q0, noshapes)
