package org.softlang.s2s.test

import org.junit.Test

// Test cases for a sublanguage of SCCQ (fully-typed SCCQ).
// A fully-typed SCCQ is a concept SCCQ where also for each
// pattern (x,y) : p there exists a pattern (x : A) and (y : B)
// in q.

class FullyTypedSCCQTests extends ValidationTests:

  val q0 =
    query("?x a :B . ?x :q ?y . ?y a :B", "?x a : A . ?x :r ?y . ?y a :A")

  @Test def t_0_0(): Unit =
    test(
      noshapes,
      q0,
      Set(
        "∃:q.⊤ ⊑ :B",
        "∃:q.⊤ ⊑ ∀-:q.:B",
        "∃:q.⊤ ⊑ ∃:q.:B",
        "∃-:q.⊤ ⊑ :B",
        "∃-:q.⊤ ⊑ ∃-:q.:B",
        "∃-:q.⊤ ⊑ ∀:q.:B"
      )
    )

  val q1 =
    query("?x a :C . ?x :q ?y . ?y a :D", "?x a : A . ?x :r ?y . ?y a :B")

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

  @Test def t_1_0(): Unit =
    test(noshapes, q1, q1_basicshapes)

  // @Test def t_1_1(): Unit =
  //  test(Set(":A ⊑ :B"), q1, q1_basicshapes)

  // @Test def t_1_2(): Unit =
  //  test(Set(":B ⊑ :A"), q1, q1_basicshapes)

  // @Test def t_1_3(): Unit =
  //  test(Set(":B ⊑ :A", ":A ⊑ :B"), q1, q1_basicshapes)

  // @Test def t_1_4(): Unit =
  //  test(Set(":A ⊑ ∃:q.:B"), q1, q1_basicshapes)

  // @Test def t_1_5(): Unit =
  //  test(
  //    Set(":A ⊑ ∃:r.:B", ":B ⊑ :A"),
  //    q1,
  //    q1_basicshapes.union(
  //      Set(
  //        ":D ⊑ ∃:q.:C",
  //        ":D ⊑ :C",
  //        ":D ⊑ ∃:q.:D",
  //        ":C ⊑ ∃:q.:C",
  //        "∃:q.⊤ ⊑ ∃:q.:C",
  //        "∃-:q.⊤ ⊑ :C",
  //        "∃-:q.⊤ ⊑ ∀:q.:C",
  //        "∃-:q.⊤ ⊑ ∃:q.:C",
  //        "∃-:q.⊤ ⊑ ∃:q.:D"
  //      )
  //    )
  //  )

  // @Test def t_1_6(): Unit =
  //  test(Set(":B ⊑ ∃:r.:A"), q1, q1_basicshapes)

  // val q2 =
  //  query("?x a :D . ?x :q ?y . ?y a :C", "?x a : A . ?x :r ?y . ?y a :B")

  // @Test def t_2_0(): Unit =
  //  test(
  //    noshapes,
  //    q2,
  //    Set(
  //      ":D ⊑ ∃:q.:C",
  //      ":C ⊑ ∃-:q.:D",
  //      "∃:q.⊤ ⊑ :D",
  //      "∃:q.⊤ ⊑ ∃:q.:C",
  //      "∃:q.⊤ ⊑ ∀-:q.:D",
  //      "∃-:q.⊤ ⊑ :C",
  //      "∃-:q.⊤ ⊑ ∃-:q.:D",
  //      "∃-:q.⊤ ⊑ ∀:q.:C"
  //    )
  //  )
