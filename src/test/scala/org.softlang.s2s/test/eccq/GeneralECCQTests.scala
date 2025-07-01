package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for general ECCQ queries.

class GeneralECCQTests extends ValidationSuite("e_general_"):

  // Example Query (Paper)
  // {(𝑥,𝑦):(𝑒,∅), (𝑦, {⊕POI, ⊖Person)}}
  //    ⇐ {(𝑥,𝑦):𝑒}, {𝑥.name = "Smith", 𝑦:Person, 𝑒:observes}
  val q0 = gcore(
    "(x)-[e]->(y)",
    "(x)-[e]->(y)",
    where = "x.name = \"Smith\" AND y:Person AND e:observes",
    set = "y:POI",
    remove = "y:Person"
  )

  includes(
    "0_0",
    noshapes,
    q0,
    Set(
      // This is a useful output shape: Each edge with 'le:observes' has a RHS node that has 'ln:POI'.
      "le:observes ⊑ => ln:POI",
      // TODO These seem to follow -- incorrectly (?) -- from v_o_name concepts.
      "∃-kn:name.⊤ ⊑ ∀:meta_nte.le:observes",
      "∃-kn:name.⊤ ⊑ ∀:meta_etn.ln:POI",
      // This is sensible, right, but why does it not also hold for POI?
      "∃kn:name.⊤ ⊑ ∀-:meta_etn.le:observes"
    )
  )

  // // Nominals

  // val q1 = gcore("(x)", "(x)", where = "x.name = \"Smith\" AND x:Person")

  // entails("1_0", noshapes, q1, Set(
  //   // This is a useful output shape: Each edge with 'le:observes' has a RHS node that has 'ln:POI'.
  //   "ln:Person ⊑ => ln:POI",
  //   // TODO These seem to follow -- incorrectly (?) -- from v_o_name concepts.
  //   "∃-kn:name.⊤ ⊑ ∀:meta_nte.le:observes",
  //   "∃-kn:name.⊤ ⊑ ∀:meta_etn.ln:POI",
  //   // This is sensible, right, but why does it not also hold for POI?
  //   "∃kn:name.⊤ ⊑ ∀-:meta_etn.le:observes",
  //   ))
