package org.softlang.s2s.test

import org.junit.Test

// Test cases for general SCCQ.

class GeneralSCCQTests extends ValidationTests:

  val q0 = query("?x :q ?y . ?x a :B", "?x :r ?y . ?x a :A")

  @Test def p_0_0(): Unit =
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

  @Test def p_1_0(): Unit =
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

  @Test def p_2_0(): Unit =
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

  @Test def p_3_0(): Unit =
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

  @Test def p_3_1(): Unit =
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

  @Test def p_4_0(): Unit =
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
