package org.softlang.s2s.test.suites

import org.softlang.s2s.test.ValidationSuite

// Additional test cases for 'forall' quantified input shapes.
class ForallTests extends ValidationSuite:

  val q0 = query("?x a :B . ?y a :C", "?x a :A . ?x :p ?y . ?y :p ?x")

  work("general_0_0", noshapes, q0, noshapes)

  work("general_0_1", Set(":A ⊑ ∀:p.:A"), q0, Set(":B ⊑ :C", ":C ⊑ :B"))

  work("general_0_2", Set(":B ⊑ :A", ":A ⊑ ∀:p.:B"), q0, Set(":B ⊑ :C", ":C ⊑ :B"))

  val q1 = query("?y a :C . ?z a :D", "?x a :A . ?x :p ?y . ?z a :B")

  work("general_1_0", Set(":A ⊑ :B", ":A ⊑ ∀:p.:A"), q1, Set(":C ⊑ :D"))
