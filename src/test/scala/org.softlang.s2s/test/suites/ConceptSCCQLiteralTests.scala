package org.softlang.s2s.test.suites

import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName
import org.softlang.s2s.test.ValidationTestSuite

// Test cases for a sublanguage of SCCQ (Concept SCCQ), where there are only
// atomic patterns of the form (x : A) or (o : A) in pattern and template.

class ConceptSCCQLiteralTests extends ValidationTestSuite("Literal-Concept"):

  @Rule
  def name = _name
  val _name = TestName()

  // Q0 .. Q1 -- No shapes.

  val q0 = query(":b a :B", ":a a :A")

  @Test def concept_0_0(): Unit =
    test(noshapes, q0, noshapes)

  val q1 = query(":a a :B", ":a a :A")

  @Test def concept_1_0(): Unit =
    test(noshapes, q1, noshapes)

  // Q2 .. Q3 -- Basic subsumption.

  val q2 = query(":b a :B . :b a :C", ":a a :A")

  @Test def concept_2_0(): Unit =
    test(
      noshapes,
      q2,
      Set(
        ":C ⊑ :B",
        ":B ⊑ :C"
      )
    )

  val q3 = query(":a a :B . :a a :C", ":a a :A")

  @Test def concept_3_0(): Unit =
    test(
      noshapes,
      q3,
      Set(
        ":C ⊑ :B",
        ":B ⊑ :C"
      )
    )

  // Q4 .. Q6 -- Various shapes that matter not.

  val q4 = query(":a a :C . :b a :D", ":a a :A . :b a :B")

  @Test def concept_4_0(): Unit =
    test(noshapes, q4, noshapes)

  @Test def concept_4_1(): Unit =
    test(Set(":A ⊑ :B"), q4, noshapes)

  @Test def concept_4_2(): Unit =
    test(Set(":B ⊑ :A"), q4, noshapes)

  @Test def concept_4_3(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :A"), q4, noshapes)

  val q5 = query(":a a :C . :b a :D", ":a a :A . :b a :B")

  @Test def concept_5_0(): Unit =
    test(noshapes, q5, noshapes)

  val q6 = query(":a a :C . :a a :D . :b a :C", ":a a :A  . :b a :A . :b a :B")

  @Test def concept_6_0(): Unit =
    test(noshapes, q6, Set(":D ⊑ :C"))

  @Test def concept_6_1(): Unit =
    test(Set(":B ⊑ :A"), q6, Set(":D ⊑ :C"))

  @Test def concept_6_2(): Unit =
    test(Set(":A ⊑ :B"), q6, Set(":D ⊑ :C"))

  // Q7 -- Adding something changes nothing.

  val q7 = query(":a a :B . ?x a :B", "?x a :A")

  @Test def concept_7_0(): Unit =
    test(noshapes, q7, noshapes)

  // Q8 -- Adding to one side, removes one-directional subsumption.

  val q8 = query(":c a :C . ?x a :C . ?y a :D", "?x a :A . ?y a :B")

  @Test def concept_8_0(): Unit =
    test(noshapes, q8, noshapes)

  @Test def concept_8_1(): Unit =
    test(Set(":A ⊑ :B"), q8, noshapes)

  @Test def concept_8_2(): Unit =
    test(Set(":B ⊑ :A"), q8, Set(":D ⊑ :C"))

  @Test def concept_8_3(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :A"), q8, Set(":D ⊑ :C"))

  // Q9 -- Adding to both sides, removes bidirectional subsumption.

  val q9 = query(":c a :C . ?x a :C . :d a :D . ?y a :D", "?x a :A . ?y a :B")

  @Test def concept_9_0(): Unit =
    test(noshapes, q9, noshapes)

  @Test def concept_9_1(): Unit =
    test(Set(":A ⊑ :B"), q9, noshapes)

  @Test def concept_9_2(): Unit =
    test(Set(":B ⊑ :A"), q9)

  @Test def concept_9_3(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :A"), q9)

  // Q10 -- Adding the same to both sides, allows subsumption.

  val q10 = query(":c a :C . ?x a :C . :c a :D . ?y a :D", "?x a :A . ?y a :B")

  @Test def concept_10_0(): Unit =
    test(noshapes, q10, noshapes)

  @Test def concept_10_1(): Unit =
    test(Set(":A ⊑ :B"), q10, Set(":C ⊑ :D"))

  @Test def concept_10_2(): Unit =
    test(Set(":B ⊑ :A"), q10, Set(":D ⊑ :C"))

  @Test def concept_10_3(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :A"), q10, Set(":C ⊑ :D", ":D ⊑ :C"))

  // Q11 -- Creating new subsumptions, in the presence of variables.

  val q11 = query(":c a :E . :c a :C . ?x a :C . ?y a :D", "?x a :A . ?y a :B")

  @Test def concept_11_0(): Unit =
    test(noshapes, q11, Set(":E ⊑ :C"))

  @Test def concept_11_1(): Unit =
    test(Set(":A ⊑ :B"), q11, Set(":E ⊑ :C"))

  @Test def concept_11_2(): Unit =
    test(Set(":B ⊑ :A"), q11, Set(":E ⊑ :C", ":D ⊑ :C"))

  @Test def concept_11_3(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :A"), q11, Set(":E ⊑ :C", ":D ⊑ :C"))

  // Q12 -- Swapping literals.

  val q12 = query(":a a :C . ?y a :D", ":a a :A . ?x a :A . :b a :B . ?y a :B")

  @Test def concept_12_0(): Unit =
    test(noshapes, q12, noshapes)

  // @Test def concept_12_1(): Unit =
  //  test(Set(":A ⊑ :B"), q12, Set(":C ⊑ :D"), debug = true)

  // @Test def concept_12_2(): Unit =
  //  test(Set(":B ⊑ :A"), q12, Set(":E ⊑ :C", ":D ⊑ :C"))

  // @Test def concept_12_3(): Unit =
  //  test(Set(":A ⊑ :B", ":B ⊑ :A"), q12, Set(":E ⊑ :C", ":D ⊑ :C"))
