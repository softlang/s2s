package org.softlang.shass.test

import org.junit.Test

class ConceptsTests extends ValidationTests:

  val q0 = query("?x a :B", "?x a :A")

  @Test def c_0_0(): Unit =
    test(noshapes, q0, noshapes)

  val q1 = query("?x a :C . ?y a :D", "?x a :A . ?y a :B")

  @Test def c_1_0(): Unit =
    test(noshapes, q1, noshapes)

  @Test def c_1_1(): Unit =
    test(Set(":A ⊑ :B"), q1, Set(":C ⊑ :D"))

  @Test def c_1_2(): Unit =
    test(Set(":B ⊑ :A"), q1, Set(":D ⊑ :C"))

  @Test def c_1_3(): Unit =
    test(
      Set(":A ⊑ :B", ":B ⊑ :A"),
      q1,
      Set(":C ⊑ :D", ":D ⊑ :C")
    )

  val q2 = query("?x a :C . ?x a :D", "?x a :A . ?x a :B")

  @Test def c_2_0(): Unit =
    test(noshapes, q2, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q3 = query("?x a :C . ?x a :D", "?y a :A . ?x a :B")

  @Test def c_3_0(): Unit =
    test(noshapes, q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  @Test def c_3_1(): Unit =
    test(Set(":B ⊑ :A"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  @Test def c_3_2(): Unit =
    test(Set(":A ⊑ :B"), q3, Set(":C ⊑ :D", ":D ⊑ :C"))

  val q4 = query(
    "?x a :D . ?z a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  @Test def c_4_0(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :C"), q4, Set(":D ⊑ :F"))

  val q5 = query(
    "?z a :D . ?x a :F",
    "?x a :A . ?y a :B . ?z a :C"
  )

  @Test def c_5_0(): Unit =
    test(Set(":A ⊑ :B", ":B ⊑ :C"), q5, Set(":F ⊑ :D"))
