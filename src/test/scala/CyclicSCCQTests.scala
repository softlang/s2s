package org.softlang.s2s.test

import org.junit.Test
import org.junit.Rule
import org.junit.rules.TestName

// Test cases for SCCQ with cycles such as (x,x):p.
class CyclicSCCQTests extends ValidationTests("Cyclic"):

  @Rule
  def name = _name
  val _name = TestName()

  val q0 = query("?x a :C . ?y a :D", "?x a :A . ?y a :B . ?y a :E")

  @Test def cyclic_0_0(): Unit =
    test(noshapes, q0, noshapes)

  @Test def cyclic_0_1(): Unit =
    test(Set(":A ⊑ :B"), q0, noshapes)
