package org.softlang.s2s.test.suites

import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName
import org.softlang.s2s.test.ValidationTestSuite

// Test cases for a sublanguage of SCCQ (Concept SCCQ), where there are only
// atomic patterns of the form (x : A) in pattern and template.

class ConceptSCCQTests extends ValidationTestSuite("Concept"):

  @Rule
  def name = _name
  val _name = TestName()

  val q0 = query("?x a :B", "?x a :A")

  @Test def concept_0_0(): Unit =
    test(noshapes, q0, noshapes)

  val q1 = query("?x a :C . ?y a :D", "?x a :A . ?y a :B")

  @Test def concept_1_0(): Unit =
    test(noshapes, q1, noshapes)

  @Test def concept_1_1(): Unit =
    test(Set(":A ⊑ :B"), q1, Set(":C ⊑ :D"), debug = true)

  @Test def concept_1_2(): Unit =
    test(Set(":B ⊑ :A"), q1, Set(":D ⊑ :C"))

  @Test def concept_1_3(): Unit =
    test(
      Set(":A ⊑ :B", ":B ⊑ :A"),
      q1,
      Set(":C ⊑ :D", ":D ⊑ :C")
    )

  val q2 = query("?x a :C . ?x a :D", "?x a :A . ?x a :B")

  @Test def concept_2_0(): Unit =
    test(noshapes, q2, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q3 = query("?x a :C . ?x a :D", "?y a :A . ?x a :B")

  @Test def concept_3_0(): Unit =
    test(noshapes, q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  @Test def concept_3_1(): Unit =
    test(Set(":B ⊑ :A"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  @Test def concept_3_2(): Unit =
    test(Set(":A ⊑ :B"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q4 = query("?x a :B . ?y a :B", "?x a :A . ?y a :A")

  @Test def concept_4_0(): Unit =
    test(noshapes, q4, noshapes)

  val q5 = query("?x a :B . ?x a :B", "?x a :A . ?x a :A")

  @Test def concept_5_0(): Unit =
    test(noshapes, q5, noshapes)

  val q6 = query(
    "?x a :D . ?z a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  @Test def concept_6_0(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :C"), q6, Set(":D ⊑ :F"))

  val q7 = query(
    "?z a :D . ?x a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  @Test def concept_7_0(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :C"), q7, Set(":F ⊑ :D"))

  val q8 = query(
    "?x a :C . ?y a :D",
    "?x a :A . ?y a :A . ?z a :B"
  )

  @Test def concept_8_0(): Unit =
    test(noshapes, q8, Set(":D ⊑ :C", ":C ⊑ :D"))

  val q9 = query(
    "?x a :D . ?y a :E . ?z a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  @Test def concept_9_0(): Unit =
    test(Set(":A ⊑ :B"), q9, Set(":D ⊑ :E"))

  @Test def concept_9_1(): Unit =
    test(Set(":B ⊑ :C"), q9, Set(":E ⊑ :F"))

  @Test def concept_9_2(): Unit =
    test(Set(":C ⊑ :B"), q9, Set(":F ⊑ :E"))

  @Test def concept_9_3(): Unit =
    test(Set(":B ⊑ :A"), q9, Set(":E ⊑ :D"))

  @Test def concept_9_4(): Unit =
    test(Set(":A ⊑ :C"), q9, Set(":D ⊑ :F"))

  @Test def concept_9_5(): Unit =
    test(Set(":C ⊑ :A"), q9, Set(":F ⊑ :D"))

  @Test def concept_9_6(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :C"), q9, Set(":D ⊑ :E", ":E ⊑ :F", ":D ⊑ :F"))
