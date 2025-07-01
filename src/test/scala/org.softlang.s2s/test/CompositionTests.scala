package org.softlang.s2s.test

/** Testing shape inference over compositions of queries. */
class CompositionTests extends ValidationSuite("compose"):
  val x = 42

  val q0 =
    List(
      gcore(
        construct = "(n)",
        matc = "(n)",
        where = "n:A"
      ),
      gcore(
        construct = "(n)",
        matc = "(n)",
        where = "n:A"
      )
  )

  compositionEntails("0_0", noshapes, q0, noshapes)
