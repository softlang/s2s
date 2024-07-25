package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple Concept ECCQ queries.

class EdgesECCQTests extends ValidationSuite("e_edges_"):

  // Basic construction cases with etn/nte.

  val q0 = gcore("(x)-[e]->(y)", "(x)-[e]->(y)", where = "x:A AND y:B AND e:k")
  val s0out = Set("le:k ⊑ => ln:B", "le:k ⊑ <= ln:A")

  // We get these basic structural shapes.
  includes("0_0", noshapes, q0, s0out)

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
    "le:j ⊑ <= ln:A"
  )

  // Since nodes fall together here, we again get the shapes from q0 (for both labels).
  includes("2_0", noshapes, q2, s2out)
