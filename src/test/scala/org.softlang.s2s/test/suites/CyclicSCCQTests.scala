package org.softlang.s2s.test.suites

import org.softlang.s2s.test.ValidationSuite

// Test cases for SCCQ with cycles such as (x,x):p.
class CyclicSCCQTests extends ValidationSuite:

  val q0 = query("?x a :C . ?y a :D", "?x a :A . ?y a :B . ?y a :E")

  work("cyclic_0_0", noshapes, q0, noshapes)

  work("cyclic_0_1", Set(":A ⊑ :B"), q0, noshapes)
