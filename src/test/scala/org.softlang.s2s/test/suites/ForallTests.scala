package org.softlang.s2s.test.suites

import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName
import org.softlang.s2s.test.ValidationTestSuite

// Additional test cases for 'forall' quantified input shapes.
class ForallTests extends ValidationTestSuite("Forall"):

  @Rule
  def name = _name
  val _name = TestName()

  val q0 = query("?x a :B . ?y a :C", "?x a :A . ?x :p ?y . ?y :p ?x")

  @Test def general_0_0(): Unit =
    test(noshapes, q0, noshapes)

  @Test def general_0_1(): Unit =
    test(Set(":A ⊑ ∀:p.:A"), q0, Set(":B ⊑ :C", ":C ⊑ :B"))

  @Test def general_0_2(): Unit =
    test(Set(":B ⊑ :A", ":A ⊑ ∀:p.:B"), q0, Set(":B ⊑ :C", ":C ⊑ :B"))

  val q1 = query("?y a :C . ?z a :D", "?x a :A . ?x :p ?y . ?z a :B")

  @Test def general_1_0(): Unit =
    test(Set(":A ⊑ :B", ":A ⊑ ∀:p.:A"), q1, Set(":C ⊑ :D"))
