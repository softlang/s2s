package org.softlang.s2s.test.suites

import org.softlang.s2s.test.ValidationSuite

// Test cases for a sublanguage of SCCQ (Concept SCCQ), where there are only
// atomic patterns of the form (x : A) or (o : A) in pattern and template.

class ConceptSCCQLiteralTests extends ValidationSuite:

  // Q0 .. Q1 -- No shapes.

  val q0 = query(":b a :B", ":a a :A")

  work("concept_0_0", noshapes, q0, noshapes)

  val q1 = query(":a a :B", ":a a :A")

  work("concept_1_0", noshapes, q1, noshapes)

  // Q2 .. Q3 -- Basic subsumption.

  val q2 = query(":b a :B . :b a :C", ":a a :A")

  work(
    "concept_2_0",
    noshapes,
    q2,
    Set(
      ":C ⊑ :B",
      ":B ⊑ :C"
    )
  )

  val q3 = query(":a a :B . :a a :C", ":a a :A")

  work(
    "concept_3_0",
    noshapes,
    q3,
    Set(
      ":C ⊑ :B",
      ":B ⊑ :C"
    )
  )

  // Q4 .. Q6 -- Various shapes that matter not.

  val q4 = query(":a a :C . :b a :D", ":a a :A . :b a :B")

  work("concept_4_0", noshapes, q4, noshapes)

  work("concept_4_1", Set(":A ⊑ :B"), q4, noshapes)

  work("concept_4_2", Set(":B ⊑ :A"), q4, noshapes)

  work("concept_4_3", Set(":A ⊑ :B", ":B ⊑ :A"), q4, noshapes)

  val q5 = query(":a a :C . :b a :D", ":a a :A . :b a :B")

  work("concept_5_0", noshapes, q5, noshapes)

  val q6 = query(":a a :C . :a a :D . :b a :C", ":a a :A  . :b a :A . :b a :B")

  work("concept_6_0", noshapes, q6, Set(":D ⊑ :C"))

  work("concept_6_1", Set(":B ⊑ :A"), q6, Set(":D ⊑ :C"))

  work("concept_6_2", Set(":A ⊑ :B"), q6, Set(":D ⊑ :C"))

  // Q7 -- Adding something changes nothing.

  val q7 = query(":a a :B . ?x a :B", "?x a :A")

  work("concept_7_0", noshapes, q7, noshapes)

  // Q8 -- Adding to one side, removes one-directional subsumption.

  val q8 = query(":c a :C . ?x a :C . ?y a :D", "?x a :A . ?y a :B")

  work("concept_8_0", noshapes, q8, noshapes)

  work("concept_8_1", Set(":A ⊑ :B"), q8, noshapes)

  work("concept_8_2", Set(":B ⊑ :A"), q8, Set(":D ⊑ :C"))

  work("concept_8_3", Set(":A ⊑ :B", ":B ⊑ :A"), q8, Set(":D ⊑ :C"))

  // Q9 -- Adding to both sides, removes bidirectional subsumption.

  val q9 = query(":c a :C . ?x a :C . :d a :D . ?y a :D", "?x a :A . ?y a :B")

  work("concept_9_0", noshapes, q9, noshapes)

  work("concept_9_1", Set(":A ⊑ :B"), q9, noshapes)

  work("concept_9_2", Set(":B ⊑ :A"), q9)

  work("concept_9_3", Set(":A ⊑ :B", ":B ⊑ :A"), q9)

  // Q10 -- Adding the same to both sides, allows subsumption.

  val q10 = query(":c a :C . ?x a :C . :c a :D . ?y a :D", "?x a :A . ?y a :B")

  work("concept_10_0", noshapes, q10, noshapes)

  work("concept_10_1", Set(":A ⊑ :B"), q10, Set(":C ⊑ :D"))

  work("concept_10_2", Set(":B ⊑ :A"), q10, Set(":D ⊑ :C"))

  work("concept_10_3", Set(":A ⊑ :B", ":B ⊑ :A"), q10, Set(":C ⊑ :D", ":D ⊑ :C"))

  // Q11 -- Creating new subsumptions, in the presence of variables.

  val q11 = query(":c a :E . :c a :C . ?x a :C . ?y a :D", "?x a :A . ?y a :B")

  work("concept_11_0", noshapes, q11, Set(":E ⊑ :C"))

  work("concept_11_1", Set(":A ⊑ :B"), q11, Set(":E ⊑ :C"))

  work("concept_11_2", Set(":B ⊑ :A"), q11, Set(":E ⊑ :C", ":D ⊑ :C"))

  work("concept_11_3", Set(":A ⊑ :B", ":B ⊑ :A"), q11, Set(":E ⊑ :C", ":D ⊑ :C"))

  // Q12 -- Swapping literals.

  val q12 = query(":a a :C . ?y a :D", ":a a :A . ?x a :A . :b a :B . ?y a :B")

  work("concept_12_0", noshapes, q12, noshapes)

  //work("concept_12_1", Set(":A ⊑ :B"), q12, Set(":C ⊑ :D"))

  //work("concept_12_2", Set(":B ⊑ :A"), q12, Set(":E ⊑ :C", ":D ⊑ :C"))

  //work("concept_12_3", Set(":A ⊑ :B", ":B ⊑ :A"), q12, Set(":E ⊑ :C", ":D ⊑ :C"))
