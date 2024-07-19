package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple Concept ECCQ queries.

class GeneralECCQTests extends ValidationSuite("e-general"):

  // Example Query (Paper)
  // {(𝑥,𝑦):(𝑒,∅), (𝑦, {⊕POI, ⊖Person)}}
  //    ⇐ {(𝑥,𝑦):𝑒}, {𝑥.name = "Smith", 𝑦:Person, 𝑒:observes}
  val q0 = gcore("(x)-[e]->(y)", "(x)-[e]->(y)", where = "x.name = \"Smith\" AND y:Person AND e:observes", set = "y:POI", remove = "y:Person")

  includes("", noshapes, q0, Set(
    // This is a useful output shape: Each edge with 'le:observes' has a RHS node that has 'ln:POI'.
    "le:observes ⊑ ∃:meta_etn.ln:POI",
    // These seem to follow -- incorrectly (?) -- from v_o_name concepts.
    "∃-kn:name.⊤ ⊑ ∀:meta_nte.le:observes",
    "∃-kn:name.⊤ ⊑ ∀:meta_etn.ln:POI",
    // This is sensible, right, but why does it not also hold for POI?
    "∃kn:name.⊤ ⊑ ∀-:meta_etn.le:observes",
    ))
