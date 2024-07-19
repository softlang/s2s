package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple Concept ECCQ queries.

class EdgesECCQTests extends ValidationSuite("e-edges"):

  // Basic construction cases with etn/nte.

  val q0 = gcore("(x)-[e]->(y)", "(x)-[e]->(y)", where = "x:A AND y:B AND e:k")
  val s0out = Set("le:k ⊑ ∃:meta_etn.ln:B", "le:k ⊑ ∃-:meta_nte.ln:A")

  includes("", noshapes, q0, s0out) // we get these basic structural shapes

  val q1 = gcore("(x)-[e]->(y), (w)-[f]->(z)", "(x)-[e]->(y), (w)-[f]->(z)", where = "x:A AND y:B AND e:k AND w:C AND z:D AND f:j")

  includes("", noshapes, q1, noshapes) // here, we loose both, as either e and f could have any edge label

  val q2 = gcore("(x)-[e]->(y), (x)-[f]->(y)", "(x)-[e]->(y), (x)-[f]->(y)", where = "x:A AND y:B AND e:k AND f:j")
  val s2out = Set("le:k ⊑ ∃:meta_etn.ln:B", "le:k ⊑ ∃-:meta_nte.ln:A", "le:j ⊑ ∃:meta_etn.ln:B", "le:j ⊑ ∃-:meta_nte.ln:A")

  includes("", noshapes, q2, s2out) // since nodes fall together here, we again get the shapes from q0 (for both labels)
