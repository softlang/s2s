package org.softlang.s2s.test.suites

import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName
import org.softlang.s2s.test.ValidationTestSuite

// Test cases for a sublanguage of SCCQ (fully-typed SCCQ).
// A fully-typed SCCQ is a concept SCCQ where also for each
// pattern (x,y) : p there exists a pattern (x : A) and (y : B)
// in q.

class FullyTypedSCCQTests extends ValidationTestSuite("Fully-Typed"):

  @Rule
  def name = _name
  val _name = TestName()

  // Q0 -- Basic, fully typed query with a single concept.

  val q0 =
    query("?x a :B . ?x :q ?y . ?y a :B", "?x a :A . ?x :r ?y . ?y a :A")

  val q0_basicshapes = Set(
    "∃:q.⊤ ⊑ :B",
    "∃:q.⊤ ⊑ ∀-:q.:B",
    "∃:q.⊤ ⊑ ∃:q.:B",
    "∃-:q.⊤ ⊑ :B",
    "∃-:q.⊤ ⊑ ∃-:q.:B",
    "∃-:q.⊤ ⊑ ∀:q.:B"
  )

  @Test def typed_0_0(): Unit =
    test(noshapes, q0, q0_basicshapes)

  val q0_guaranteed_r = Set(":B ⊑ ∃:q.:B", "∃-:q.⊤ ⊑ ∃:q.:B")

  @Test def typed_0_1(): Unit =
    test(Set(":A ⊑ ∃:r.:A"), q0, q0_basicshapes.union(q0_guaranteed_r))

  val q0_guaranteed_mr = Set(":B ⊑ ∃-:q.:B", "∃:q.⊤ ⊑ ∃-:q.:B")

  @Test def typed_0_2(): Unit =
    test(Set(":A ⊑ ∃-:r.:A"), q0, q0_basicshapes.union(q0_guaranteed_mr))

  val q0_guaranteed_r_and_mr = q0_guaranteed_r.union(q0_guaranteed_mr)

  @Test def typed_0_3(): Unit =
    test(
      Set(":A ⊑ ∃-:r.:A", ":A ⊑ ∃:r.:A"),
      q0,
      q0_basicshapes.union(q0_guaranteed_r_and_mr)
    )

  // Q1 -- Fully typed query with one property (renaming).

  val q1 =
    query("?x a :C . ?x :q ?y . ?y a :D", "?x a :A . ?x :r ?y . ?y a :B")

  val q1_basicshapes = Set(
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∃:q.:D",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∃-:q.:C",
    "∃-:q.⊤ ⊑ ∀:q.:D"
  )

  @Test def typed_1_0(): Unit =
    test(noshapes, q1, q1_basicshapes)

  val q1_a_sub_b = Set(
    ":C ⊑ :D",
    ":C ⊑ ∃-:q.:D",
    ":C ⊑ ∃-:q.:C",
    ":D ⊑ ∃-:q.:D",
    "∃:q.⊤ ⊑ ∃-:q.:D",
    "∃:q.⊤ ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ :D",
    "∃:q.⊤ ⊑ ∀-:q.:D",
    "∃-:q.⊤ ⊑ ∃-:q.:D"
  )

  @Test def typed_1_1(): Unit =
    test(Set(":A ⊑ :B"), q1, q1_basicshapes)

  val q1_b_sub_a = Set(
    ":D ⊑ :C",
    ":D ⊑ ∃:q.:D",
    ":D ⊑ ∃:q.:C",
    ":C ⊑ ∃:q.:C",
    "∃-:q.⊤ ⊑ ∃:q.:C",
    "∃-:q.⊤ ⊑ ∃:q.:D",
    "∃-:q.⊤ ⊑ :C",
    "∃-:q.⊤ ⊑ ∀:q.:C",
    "∃:q.⊤ ⊑ ∃:q.:C"
  )

  @Test def typed_1_2(): Unit =
    test(Set(":B ⊑ :A"), q1, q1_basicshapes)

  val q1_a_eq_b = q1_a_sub_b.union(q1_b_sub_a)

  @Test def typed_1_3(): Unit =
    test(Set(":B ⊑ :A", ":A ⊑ :B"), q1, q1_basicshapes)

  @Test def typed_1_4(): Unit =
    test(Set(":A ⊑ ∃:r.:B"), q1, q1_basicshapes)

  @Test def typed_1_5(): Unit =
    test(Set(":A ⊑ ∃:r.:B", ":B ⊑ :A"), q1, q1_basicshapes.union(q1_b_sub_a))

  @Test def typed_1_6(): Unit =
    test(Set(":B ⊑ ∃-:r.:A"), q1, q1_basicshapes)

  @Test def typed_1_7(): Unit =
    test(Set(":B ⊑ ∃-:r.:A", ":A ⊑ :B"), q1, q1_basicshapes.union(q1_a_sub_b))

  // Q2 -- Fully typed query with one property (renaming - swapping).

  val q2 =
    query("?x a :D . ?x :q ?y . ?y a :C", "?x a :A . ?x :r ?y . ?y a :B")

  val q2_basicshapes = Set(
    ":D ⊑ ∃:q.:C",
    ":C ⊑ ∃-:q.:D",
    "∃:q.⊤ ⊑ :D",
    "∃:q.⊤ ⊑ ∃:q.:C",
    "∃:q.⊤ ⊑ ∀-:q.:D",
    "∃-:q.⊤ ⊑ :C",
    "∃-:q.⊤ ⊑ ∃-:q.:D",
    "∃-:q.⊤ ⊑ ∀:q.:C"
  )

  @Test def typed_2_0(): Unit =
    test(noshapes, q2, q2_basicshapes)

  val q2_a_sub_b = Set(
    ":D ⊑ :C",
    ":D ⊑ ∃-:q.:C",
    ":D ⊑ ∃-:q.:D",
    ":C ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ ∃-:q.:D",
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ ∃-:q.:C"
  )

  @Test def typed_2_1(): Unit =
    test(Set(":A ⊑ :B"), q2, q2_basicshapes)

  val q2_b_sub_a = Set(
    ":C ⊑ :D",
    ":C ⊑ ∃:q.:C",
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃:q.:D",
    "∃-:q.⊤ ⊑ ∃:q.:D",
    "∃-:q.⊤ ⊑ ∃:q.:C",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∀:q.:D",
    "∃:q.⊤ ⊑ ∃:q.:D"
  )

  @Test def typed_2_2(): Unit =
    test(Set(":B ⊑ :A"), q2, q2_basicshapes)

  val q2_a_eq_b = q2_a_sub_b.union(q2_b_sub_a)

  @Test def typed_2_3(): Unit =
    test(Set(":B ⊑ :A", ":A ⊑ :B"), q2, q2_basicshapes)

  @Test def typed_2_4(): Unit =
    test(Set(":A ⊑ ∃:r.:B"), q2, q2_basicshapes)

  @Test def typed_2_5(): Unit =
    test(Set(":A ⊑ ∃:r.:B", ":B ⊑ :A"), q2, q2_basicshapes.union(q2_b_sub_a))

  @Test def typed_2_6(): Unit =
    test(Set(":B ⊑ ∃-:r.:A"), q2, q2_basicshapes)

  @Test def typed_2_7(): Unit =
    test(Set(":B ⊑ ∃-:r.:A", ":A ⊑ :B"), q2, q2_basicshapes.union(q2_a_sub_b))

  // Q3 -- Dropping RHS type.

  val q3 =
    query("?x a :C . ?x :q ?y", "?x a :A . ?x :r ?y . ?y a :B")

  val q3_basicshapes = Set(
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ ∃-:q.:C"
  )

  @Test def typed_3_0(): Unit =
    test(noshapes, q3, q3_basicshapes)

  // Q4 -- Dropping LHS type.

  val q4 =
    query("?x :q ?y . ?y a :D", "?x a : A . ?x :r ?y . ?y a :B")

  val q4_basicshapes = Set(
    "∃:q.⊤ ⊑ ∃:q.:D",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∀:q.:D"
  )

  @Test def typed_4_0(): Unit =
    test(noshapes, q4, q4_basicshapes)

  // Q5 -- Adding synonym type on LHS.

  val q5 =
    query(
      "?x a :E . ?x a :C . ?x :q ?y . ?y a :D",
      "?x a :A . ?x :r ?y . ?y a :B"
    )

  val q5_basicshapes = Set(
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∃:q.:D",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∃-:q.:C",
    "∃-:q.⊤ ⊑ ∀:q.:D",
    ":C ⊑ :E",
    ":E ⊑ :C",
    ":E ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:E",
    "∃:q.⊤ ⊑ :E",
    "∃:q.⊤ ⊑ ∀-:q.:E",
    "∃-:q.⊤ ⊑ ∃-:q.:E"
  )

  @Test def typed_5_0(): Unit =
    test(noshapes, q5, q5_basicshapes)

  // QW6 -- Adding synonym type on RHS.

  val q6 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D . ?y a :E",
      "?x a :A . ?x :r ?y . ?y a :B"
    )

  val q6_basicshapes = Set(
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∃:q.:D",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∃-:q.:C",
    "∃-:q.⊤ ⊑ ∀:q.:D",
    ":D ⊑ :E",
    ":E ⊑ :D",
    ":C ⊑ ∃:q.:E",
    ":E ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ ∃:q.:E",
    "∃-:q.⊤ ⊑ :E",
    "∃-:q.⊤ ⊑ ∀:q.:E"
  )

  @Test def typed_6_0(): Unit =
    test(noshapes, q6, q6_basicshapes)

  // Q7 -- Constraining with additional type on LHS.

  val q7 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D",
      "?x a :E . ?x a : A . ?x :r ?y . ?y a :B"
    )

  @Test def typed_7_0(): Unit =
    test(noshapes, q7, q1_basicshapes)

  @Test def typed_7_1(): Unit =
    test(Set(":A ⊑ :B"), q7, q1_basicshapes)

  @Test def typed_7_2(): Unit =
    test(Set(":B ⊑ :A"), q7, q1_basicshapes)

  @Test def typed_7_3(): Unit =
    test(Set(":B ⊑ :A", ":A ⊑ :B"), q7, q1_basicshapes)

  @Test def typed_7_4(): Unit =
    test(Set(":A ⊑ ∃:r.:B"), q7, q1_basicshapes)

  @Test def typed_7_5(): Unit =
    test(
      Set(":A ⊑ ∃:r.:B", ":B ⊑ :A"),
      q7,
      q1_basicshapes,
      // atleast = q1_basicshapes.union(Set(":D ⊑ :C")),
      debug = true
    )

  @Test def typed_7_6(): Unit =
    test(Set(":B ⊑ ∃-:r.:A"), q7, q1_basicshapes)

  @Test def typed_7_7(): Unit =
    test(
      Set(":B ⊑ ∃-:r.:A", ":A ⊑ :B"),
      q7,
      q1_basicshapes,
      // atleast = q1_basicshapes.union(Set(":C ⊑ :D")),
      debug = true
    )

  // Q8 -- Constraining with additional type on LHS.

  val q8 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D",
      "?x a :A . ?x :r ?y . ?y a :B . ?y a :E"
    )

  @Test def typed_8_0(): Unit =
    test(noshapes, q8, q1_basicshapes)

  @Test def typed_8_1(): Unit =
    test(Set(":A ⊑ :B"), q8, q1_basicshapes)

  @Test def typed_8_2(): Unit =
    test(Set(":B ⊑ :A"), q8, q1_basicshapes)

  @Test def typed_8_3(): Unit =
    test(Set(":B ⊑ :A", ":A ⊑ :B"), q8, q1_basicshapes)

  @Test def typed_8_4(): Unit =
    test(Set(":A ⊑ ∃:r.:B"), q8, q1_basicshapes)

  @Test def typed_8_5(): Unit =
    test(Set(":A ⊑ ∃:r.:B", ":B ⊑ :A"), q8, q1_basicshapes)

  @Test def typed_8_6(): Unit = test(
    Set(":A ⊑ ∃:r.:B", ":B ⊑ :A", ":B ⊑ :E"),
    q8,
    q1_basicshapes.union(q1_b_sub_a)
  )

  @Test def typed_8_7(): Unit =
    test(Set(":B ⊑ ∃-:r.:A"), q8, q1_basicshapes)

  @Test def typed_8_8(): Unit =
    test(Set(":B ⊑ ∃-:r.:A", ":A ⊑ :B"), q8, q1_basicshapes)

  @Test def typed_8_9(): Unit =
    test(
      Set(":B ⊑ ∃-:r.:A", ":A ⊑ :B", ":B ⊑ :E"),
      q8,
      q1_basicshapes.union(q1_a_sub_b)
    )

  // Q9 -- Copying all LHS with additional variable.

  val q9 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D . ?z a :C",
      "?x a :A . ?x :r ?y . ?y a :B . ?z a :A"
    )

  val q9_basicshapes = q1_basicshapes.removedAll(Set(":C ⊑ ∃:q.:D"))

  @Test def typed_9_0(): Unit =
    test(noshapes, q9, q9_basicshapes)

  @Test def typed_9_1(): Unit =
    test(Set(":A ⊑ ∃:r.:B"), q9, q1_basicshapes)

  // Q10 -- Copying all RHS with additional variable.

  val q10 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D . ?z a :D",
      "?x a :A . ?x :r ?y . ?y a :B . ?z a :B"
    )

  val q10_basicshapes = q1_basicshapes.removedAll(Set(":D ⊑ ∃-:q.:C"))

  @Test def typed_10_0(): Unit =
    test(noshapes, q10, q10_basicshapes)

  @Test def typed_10_1(): Unit =
    test(Set(":B ⊑ ∃-:r.:A"), q10, q1_basicshapes)

  // Q11 -- Copying all properties with additional variables.

  val q11 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D . ?z :q ?w",
      "?x a :A . ?x :r ?y . ?y a :B . ?z :r ?w"
    )

  val q11_basicshapes = Set(
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:C"
  )

  @Test def typed_11_0(): Unit =
    test(noshapes, q11, q11_basicshapes)

  @Test def typed_11_1(): Unit =
    test(
      Set("∃:r.⊤ ⊑ :A", "∃-:r.⊤ ⊑ :B"),
      q11,
      q11_basicshapes.union(
        Set(
          "∃:q.⊤ ⊑ ∀-:q.:C",
          "∃:q.⊤ ⊑ ∃:q.:D",
          "∃:q.⊤ ⊑ :C",
          "∃-:q.⊤ ⊑ ∀:q.:D",
          "∃-:q.⊤ ⊑ :D",
          "∃-:q.⊤ ⊑ ∃-:q.:C"
        )
      )
    )

  // Q12 -- Two cases, side by side.

  val q12 =
    query(
      "?x a :C . ?x :q ?y . ?y a :D . ?u a :G . ?u :s ?v . ?v a :H",
      "?x a :A . ?x :r ?y . ?y a :B . ?u a :E . ?u :p ?v . ?v a :F"
    )

  val q12_basicshapes = Set(
    // C, D and q
    ":C ⊑ ∃:q.:D",
    ":D ⊑ ∃-:q.:C",
    "∃:q.⊤ ⊑ :C",
    "∃:q.⊤ ⊑ ∃:q.:D",
    "∃:q.⊤ ⊑ ∀-:q.:C",
    "∃-:q.⊤ ⊑ :D",
    "∃-:q.⊤ ⊑ ∃-:q.:C",
    "∃-:q.⊤ ⊑ ∀:q.:D",
    // G, H and s
    ":G ⊑ ∃:s.:H",
    ":H ⊑ ∃-:s.:G",
    "∃:s.⊤ ⊑ :G",
    "∃:s.⊤ ⊑ ∃:s.:H",
    "∃:s.⊤ ⊑ ∀-:s.:G",
    "∃-:s.⊤ ⊑ :H",
    "∃-:s.⊤ ⊑ ∃-:s.:G",
    "∃-:s.⊤ ⊑ ∀:s.:H",
    // Comined (foralls)
    // Note, that some are entailed / optimized away,
    // e.g., {∃-:s.⊤ ⊑ :H} |= ∃:q.⊤ ⊑ ∀:s.:H
    "∃-:q.⊤ ⊑ ∀:s.:H",
    "∃-:s.⊤ ⊑ ∀:q.:D",
    "∃:q.⊤ ⊑ ∀-:s.:G",
    "∃:s.⊤ ⊑ ∀-:q.:C"
  )

  @Test def typed_12_0(): Unit =
    test(noshapes, q12, q12_basicshapes)

  @Test def typed_12_1(): Unit =
    test(Set(":A ⊑ :E"), q12, q12_basicshapes)

  @Test def typed_12_2(): Unit =
    test(Set(":E ⊑ :A"), q12, q12_basicshapes)

  @Test def typed_12_3(): Unit =
    test(Set(":B ⊑ :F"), q12, q12_basicshapes)

  @Test def typed_12_4(): Unit =
    test(Set(":F ⊑ :B"), q12, q12_basicshapes)

  @Test def typed_12_6(): Unit =
    test(
      Set("∃-:r.⊤ ⊑ ∃-:p.:E", ":B ⊑ :F"),
      q12,
      atleast = q12_basicshapes.union(
        Set(
          ":D ⊑ :H"
        )
      )
    )

  @Test def typed_12_7(): Unit =
    test(Set(":A ⊑ :E", ":B ⊑ :F"), q12, q12_basicshapes)

  @Test def typed_12_8(): Unit =
    test(Set(":A ⊑ :E", ":A ⊑ :B"), q12, q12_basicshapes)

  @Test def typed_12_9(): Unit =
    test(Set(":E ⊑ :A", ":B ⊑ :A"), q12, q12_basicshapes)

  @Test def typed_13_0(): Unit =
    test(
      Set("∃:r.⊤ ⊑ ∃:p.:F", ":A ⊑ :E"),
      q12,
      atleast = q12_basicshapes.union(
        Set(
          ":C ⊑ :G"
        )
      )
    )

  @Test def typed_13_1(): Unit =
    test(
      Set("∃:r.⊤ ⊑ ∃:p.:F", "∃:p.⊤ ⊑ :E"),
      q12,
      atleast = q12_basicshapes.union(
        Set(
          ":C ⊑ :G"
        )
      )
    )

  @Test def typed_13_2(): Unit =
    test(Set("∃:r.⊤ ⊑ ∃:p.:F"), q12, q12_basicshapes)
