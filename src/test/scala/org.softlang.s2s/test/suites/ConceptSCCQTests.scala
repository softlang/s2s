package org.softlang.s2s.test.suites

import org.softlang.s2s.test.ValidationSuite

// Test cases for a sublanguage of SCCQ (Concept SCCQ), where there are only
// atomic patterns of the form (x : A) in pattern and template.

class ConceptSCCQTests extends ValidationSuite:

  val q0 = query("?x a :B", "?x a :A")

  work("concept_0_0", noshapes, q0, noshapes)

  val q1 = query("?x a :C . ?y a :D", "?x a :A . ?y a :B")

  work("concept_1_0", noshapes, q1, noshapes)

  work("concept_1_1", Set(":A ⊑ :B"), q1, Set(":C ⊑ :D"))

  work("concept_1_2", Set(":B ⊑ :A"), q1, Set(":D ⊑ :C"))

  work(
    "concept_1_3",
    Set(":A ⊑ :B", ":B ⊑ :A"),
    q1,
    Set(":C ⊑ :D", ":D ⊑ :C")
  )

  val q2 = query("?x a :C . ?x a :D", "?x a :A . ?x a :B")

  work("concept_2_0", noshapes, q2, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q3 = query("?x a :C . ?x a :D", "?y a :A . ?x a :B")

  work("concept_3_0", noshapes, q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  work("concept_3_1", Set(":B ⊑ :A"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  work("concept_3_2", Set(":A ⊑ :B"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q4 = query("?x a :B . ?y a :B", "?x a :A . ?y a :A")

  work("concept_4_0", noshapes, q4, noshapes)

  val q5 = query("?x a :B . ?x a :B", "?x a :A . ?x a :A")

  work("concept_5_0", noshapes, q5, noshapes)

  val q6 = query(
    "?x a :D . ?z a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  work("concept_6_0", Set(":A ⊑ :B", ":B ⊑ :C"), q6, Set(":D ⊑ :F"))

  val q7 = query(
    "?z a :D . ?x a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  work("concept_7_0", Set(":A ⊑ :B", ":B ⊑ :C"), q7, Set(":F ⊑ :D"))

  val q8 = query(
    "?x a :C . ?y a :D",
    "?x a :A . ?y a :A . ?z a :B"
  )

  work("concept_8_0", noshapes, q8, Set(":D ⊑ :C", ":C ⊑ :D"))

  val q9 = query(
    "?x a :D . ?y a :E . ?z a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  work("concept_9_0", Set(":A ⊑ :B"), q9, Set(":D ⊑ :E"))

  work("concept_9_1", Set(":B ⊑ :C"), q9, Set(":E ⊑ :F"))

  work("concept_9_2", Set(":C ⊑ :B"), q9, Set(":F ⊑ :E"))

  work("concept_9_3", Set(":B ⊑ :A"), q9, Set(":E ⊑ :D"))

  work("concept_9_4", Set(":A ⊑ :C"), q9, Set(":D ⊑ :F"))

  work("concept_9_5", Set(":C ⊑ :A"), q9, Set(":F ⊑ :D"))

  work("concept_9_6", Set(":A ⊑ :B", ":B ⊑ :C"), q9, Set(":D ⊑ :E", ":E ⊑ :F", ":D ⊑ :F"))
