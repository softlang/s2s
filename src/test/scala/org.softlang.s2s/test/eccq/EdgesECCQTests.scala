package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple edge ECCQ queries.

class EdgesECCQTests extends ValidationSuite("e_edges_"):

  // Basic construction cases with etn/nte.

  val q0 = gcore("(x)-[e]->(y)", "(x)-[e]->(y)", where = "x:A AND y:B AND e:k")
  val s0out = Set("le:k ⊑ => ln:B", "le:k ⊑ <= ln:A")

  // We get these basic structural shapes.
  includes("0_0", noshapes, q0, s0out)
  includes("0_1", Set("ln:A ⊑ ln:B"), q0, s0out)
  includes("0_2", Set("ln:B ⊑ ln:A"), q0, s0out)

  val q1 = gcore("(x)-[e]->(y), (w)-[f]->(z)", "(x)-[e]->(y), (w)-[f]->(z)",
                 where = "x:A AND y:B AND e:k AND w:C AND z:D AND f:j")

  // Here, we loose both, as either e and f could have any edge label.
  includes("1_0", noshapes, q1, noshapes)

  val q2 = gcore("(x)-[e]->(y), (x)-[f]->(y)", "(x)-[e]->(y), (x)-[f]->(y)",
                 where = "x:A AND y:B AND e:k AND f:j")

  val s2out = Set(
    "le:k ⊑ => ln:B",
    "le:k ⊑ <= ln:A",
    "le:j ⊑ => ln:B",
    "le:j ⊑ <= ln:A",
  )

  // Since nodes fall together here, we again get the shapes from q0 (for both labels).
  includes("2_0", noshapes, q2, s2out)

  val s3in = Set(
    "ln:A ⊑ ln:C",
    "ln:C ⊑ ln:A",
    "ln:B ⊑ ln:D",
    "ln:D ⊑ ln:B",
  )

  val q4 = gcore("(x)-[e]->(y)", "(x)-[e]->(y)",
               where = "x:A AND y:B AND e:k")
  val q4e = gcore("(x)-[e]->(y)", "(x)-[e]->(y)",
                where = "x:A AND y:B AND e:k     AND     x:B AND y:A") // TODO adding this solves the issue
  val q4eA = gcore("(x)-[e]->(y)", "(x)-[e]->(y)",
                where = "x:A AND y:B AND e:k     AND     x:B") // TODO adding this solves the issue
  val q4eB = gcore("(x)-[e]->(y)", "(x)-[e]->(y)",
                where = "x:A AND y:B AND e:k     AND     y:A") // TODO adding this solves the issue

  val s4in = Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A")
  val s4inA = Set("ln:A ⊑ ln:B")
  val s4inB = Set("ln:B ⊑ ln:A")

  val s4out = Set(
    "le:k ⊑ => ln:B",
    "le:k ⊑ <= ln:A",
    "le:k ⊑ => ln:A",
    "le:k ⊑ <= ln:B",
    "ln:A ⊑ ln:B",
    "ln:B ⊑ ln:A",
  )

  val s4outA = s4out.diff(
    Set(
      "ln:B ⊑ ln:A",
      "le:k ⊑ => ln:A",
    ))

  val s4outB = s4out.diff(
    Set(
      "ln:A ⊑ ln:B",
      "le:k ⊑ <= ln:B",
    ))

  includes("4_0", s4in, q4e, s4out)
  includes("4_1", s4in, q4, s4out)
  includes("4_2", s4inA, q4eA, s4outA)
  includes("4_3", s4inA, q4e, s4outA)
  includes("4_4", s4inB, q4eB, s4outB)
  includes("4_5", s4inA, q4e, s4outB)

  val q5 = gcore("(x)-[e]->(y), (w)-[f]->(z)", "(x)-[e]->(y), (w)-[f]->(z)",
                 where = "x:A AND y:B AND e:k AND w:C AND z:D AND f:j")
  val q5e = gcore("(x)-[e]->(y), (w)-[f]->(z)", "(x)-[e]->(y), (w)-[f]->(z)",
                 where = "x:A AND y:B AND e:k AND w:C AND z:D AND f:j   AND   x:C AND y:D AND w:A AND z:B")

  val s5in = Set(
    "ln:A ⊑ ln:C",
    "ln:C ⊑ ln:A",
    "ln:B ⊑ ln:D",
    "ln:D ⊑ ln:B",
  )

  val s5out = Set(
    "le:k ⊑ => ln:B",
    "le:k ⊑ <= ln:A",
    "le:j ⊑ => ln:B",
    "le:j ⊑ <= ln:A",
    "le:k ⊑ => ln:D",
    "le:k ⊑ <= ln:C",
    "le:j ⊑ => ln:D",
    "le:j ⊑ <= ln:C",
  )

  includes("5_0", s5in, q5e, s5out)
  includes("5_1", s5in, q5, s5out)

  // // TODO Error -- see also
  // val q13 = gcore("(x)", "(x)", where = "x:A")
  // includes("13_1", Set("ln:A ⊑ ln:B"), q13, Set("ln:A ⊑ ln:B"))
