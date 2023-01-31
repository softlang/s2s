package org.softlang.s2s.test.suites

import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName
import org.softlang.s2s.test.ValidationTestSuite

// Test cases for general SCCQ.
class GeneralSCCQTests extends ValidationTestSuite("General"):

  @Rule
  def name = _name
  val _name = TestName()

  val q0 = query("?x :q ?y . ?x a :B", "?x :r ?y . ?x a :A")

  @Test def general_0_0(): Unit =
    test(
      noshapes,
      q0,
      Set(
        "∃:q.⊤ ⊑ :B",
        "∃:q.⊤ ⊑ ∀-:q.:B",
        "∃-:q.⊤ ⊑ ∃-:q.:B"
      )
    )

  val q1 = query("?x :q ?y . ?y a :B", "?x :r ?y . ?y a :A")

  @Test def general_1_0(): Unit =
    test(
      noshapes,
      q1,
      Set(
        "∃:q.⊤ ⊑ ∃:q.:B",
        "∃-:q.⊤ ⊑ ∀:q.:B",
        "∃-:q.⊤ ⊑ :B"
      )
    )

  val q2 = query(
    "?x a :B . ?x :q ?y . ?y a :B",
    "?x a :A . ?x :r ?y . ?y a :A"
  )

  @Test def general_2_0(): Unit =
    test(
      noshapes,
      q2,
      Set(
        "∃:q.⊤ ⊑ :B",
        "∃:q.⊤ ⊑ ∃:q.:B",
        "∃:q.⊤ ⊑ ∀-:q.:B",
        "∃-:q.⊤ ⊑ :B",
        "∃-:q.⊤ ⊑ ∃-:q.:B",
        "∃-:q.⊤ ⊑ ∀:q.:B"
      )
    )

  val q3 = query(
    "?x a :C . ?x :q ?y . ?y a :D",
    "?x a :A . ?x :r ?y . ?y a :B"
  )

  @Test def general_3_0(): Unit =
    test(
      noshapes,
      q3,
      Set(
        ":C ⊑ ∃:q.:D",
        ":D ⊑ ∃-:q.:C",
        "∃:q.⊤ ⊑ :C",
        "∃:q.⊤ ⊑ ∃:q.:D",
        "∃:q.⊤ ⊑ ∀-:q.:C",
        "∃-:q.⊤ ⊑ :D",
        "∃-:q.⊤ ⊑ ∃-:q.:C",
        "∃-:q.⊤ ⊑ ∀:q.:D"
      )
    )

  @Test def general_3_1(): Unit =
    test(
      Set(
        ":A ⊑ :B",
        ":B ⊑ :A"
      ),
      q3,
      Set(
        ":C ⊑ ∃:q.:D",
        ":D ⊑ ∃-:q.:C",
        "∃:q.⊤ ⊑ :C",
        "∃:q.⊤ ⊑ ∃:q.:D",
        "∃:q.⊤ ⊑ ∀-:q.:C",
        "∃-:q.⊤ ⊑ :D",
        "∃-:q.⊤ ⊑ ∃-:q.:C",
        "∃-:q.⊤ ⊑ ∀:q.:D"
      )
    )

  val q4 = query("?x :q ?x . ?x a :B", "?x :r ?x . ?x a :A")

  @Test def general_4_0(): Unit =
    test(
      noshapes,
      q4,
      Set(
        "∃:q.⊤ ⊑ :B",
        "∃:q.⊤ ⊑ ∃:q.:B",
        "∃:q.⊤ ⊑ ∀-:q.:B",
        "∃-:q.⊤ ⊑ :B",
        "∃-:q.⊤ ⊑ ∃-:q.:B",
        "∃-:q.⊤ ⊑ ∀:q.:B",
        ":B ⊑ ∃-:q.:B",
        "∃:q.⊤ ⊑ ∃-:q.:B",
        "∃-:q.⊤ ⊑ ∃:q.:B",
        ":B ⊑ ∃:q.:B"
      )
    )

  val q5 = query("?x :q ?y . ?z a :B", "?x :r ?y . ?z a :A")

  @Test def general_5_0(): Unit =
    test(noshapes, q5, noshapes)

  @Test def general_5_1(): Unit =
    test(
      Set("∃:r.⊤ ⊑ :A"),
      q5,
      Set(
        "∃:q.⊤ ⊑ :B",
        "∃:q.⊤ ⊑ ∀-:q.:B",
        "∃-:q.⊤ ⊑ ∃-:q.:B"
      )
    )

  val q6 = query("?x a :B . ?y a :C", "?x :p ?x . ?y a :A")

  @Test def general_6_0(): Unit =
    test(Set(":A ⊑ ∃:p.:A"), q6, noshapes)

  val q7 = query("?x a :B . ?y a :C", "?y :q ?x . ?x a :A")

  @Test def general_7_0(): Unit =
    test(Set("∃:q.⊤ ⊑ :A"), q7, noshapes)

  val q8 = query("?x a :B . ?y a :C", "?y :q ?x . ?x :p ?y . ?y a :A")

  @Test def general_8_0(): Unit =
    test(Set("∃:q.⊤ ⊑ ∃:p.:A"), q8, noshapes)

  val q9 = query("?x a :B . ?y a :C", "?y :p ?x . ?x :p ?y")

  @Test def general_9_0(): Unit =
    test(noshapes, q9, Set(":B ⊑ :C", ":C ⊑ :B"))

  val q10 = query("?x :l ?y . ?z a :D", "?x :l ?y . ?z a :D")

  @Test def general_10_0(): Unit =
    test(Set(":D ⊑ ∃:l.:D"), q10, Set(":D ⊑ ∃:l.:D"))
