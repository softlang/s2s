package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for general ECCQ queries.

class BlankNodeTests extends ValidationSuite("e_blank_"):

  // Fresh edge variable.

  val q0 = gcore(
    "(x)-[e]->(y)",
    "(x), (y)",
    where = "x:Person AND y:Person",
    set = "e:sameSpecies",
    remove = ""
  )

  includes(
    "0_0",
    noshapes,
    q0,
    Set(
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:Person)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:Person)",
      "ln:Person ⊑ ∃:meta_nte.(le:sameSpecies)",
      "ln:Person ⊑ ∃-:meta_etn.(le:sameSpecies)"
    )
  )

  val q1 = gcore(
    "(x)-[e]->(y)",
    "(x), (y)",
    where = "x:Person AND y:Agent",
    set = "e:sameSpecies AND x:A",
    remove = "x:Person"
  )

  includes(
    "1_0",
    noshapes,
    q1,
    Set(
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:A)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:Agent)",
      // Note: No "ln:A ⊑ ∃:meta_nte.(le:sameSpecies)"
      // because y might also have 'A'
      "ln:Agent ⊑ ∃-:meta_etn.(le:sameSpecies)"
    )
  )

  includes(
    "1_1",
    Set(
      "ln:Person ⊑ ln:Agent"
    ),
    q1,
    Set(
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:A)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:Agent)",
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:Agent)", // because Person ⊑ Agent
      "ln:Agent ⊑ ∃-:meta_etn.(le:sameSpecies)",
      "ln:A ⊑ ln:Agent", // dito
      "ln:A ⊑ ∃-:meta_etn.(le:sameSpecies)" // dito
    )
  )

  includes(
    "1_2",
    Set(
      "ln:Agent ⊑ ln:Person"
    ),
    q1,
    Set(
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:A)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:Agent)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:A)", // because Agent ⊑ Person
      "ln:Agent ⊑ ∃-:meta_etn.(le:sameSpecies)",
      "ln:Agent ⊑ ln:A", // dito
      "ln:A ⊑ ∃:meta_nte.(le:sameSpecies)", // dito
      "ln:Agent ⊑ ∃:meta_nte.(le:sameSpecies)" // dito
    )
  )

  includes(
    "1_3",
    Set(
      "ln:Agent ⊑ ln:Person",
      "ln:Person ⊑ ln:Agent"
    ),
    q1,
    // Essentially with the combined reasoning of the cases above.
    Set(
      // Equality persists.
      "ln:A ⊑ ln:Agent",
      "ln:Agent ⊑ ln:A",
      //
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:A)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:Agent)",
      "le:sameSpecies ⊑ ∃:meta_etn.(ln:A)",
      "le:sameSpecies ⊑ ∃-:meta_nte.(ln:Agent)",
      //
      "ln:A ⊑ ∃-:meta_etn.(le:sameSpecies)",
      "ln:A ⊑ ∃:meta_nte.(le:sameSpecies)",
      "ln:Agent ⊑ ∃-:meta_etn.(le:sameSpecies)",
      "ln:Agent ⊑ ∃:meta_nte.(le:sameSpecies)"
    )
  )

  // Fresh node variable.

  val q2 = gcore("(x), (y)", "(x)")

  includes("2_0", noshapes, q2, noshapes)

  includes(
    "2_1",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q2,
    Set(
      "ln:A ⊑ ln:B"
    )
  )

  includes(
    "2_2",
    Set(
      "ln:A ⊑ ln:B",
      "ln:C ⊑ ln:D"
    ),
    q2,
    Set(
      "ln:A ⊑ ln:B",
      "ln:C ⊑ ln:D"
    )
  )

  includes(
    "2_3",
    Set(
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:C"
    ),
    q2,
    Set(
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:C",
      "ln:A ⊑ ln:C"
    )
  )

  val q3 = gcore(
    "(x), (y)",
    "(x)",
    where = "x:A"
  )

  includes("3_0", noshapes, q3, noshapes)

  includes(
    "3_1",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q3,
    Set(
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:A"
    )
  )

  includes(
    "3_2",
    Set(
      "ln:B ⊑ ln:A"
    ),
    q3,
    Set(
      "ln:B ⊑ ln:A"
    )
  )

  includes(
    "3_3",
    Set(
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:C"
    ),
    q3,
    Set(
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:A",
      "ln:B ⊑ ln:C",
      "ln:C ⊑ ln:B",
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  val q4 = gcore(
    "(x), (y)",
    "(x)",
    set = "y:B AND y:C"
  )

  // Note: Since x might have only B or only C, no shapes result here!

  includes("4_0", noshapes, q4, noshapes)

  includes(
    "4_1",
    Set(
      "ln:B ⊑ ln:C"
    ),
    q4,
    Set(
      "ln:B ⊑ ln:C"
    )
  )

  includes(
    "4_2",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q4,
    Set(
      "ln:A ⊑ ln:B"
    )
  )

  // The following shape gets invalidated by the presence of
  // new nodes labelled with B or C, respectively.
  includes(
    "4_3",
    Set(
      "ln:B ⊑ ln:A",
      "ln:C ⊑ ln:A"
    ),
    q4,
    noshapes,
    debugging = true
  )

  val q5 = gcore(
    "(x), (y)",
    "(x)",
    where = "x:A",
    set = "y:B AND y:C"
  )

  includes(
    "5_0",
    noshapes,
    q5,
    noshapes
  )

  includes(
    "5_1",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q5,
    Set(
      "ln:A ⊑ ln:B",
      "ln:C ⊑ ln:B" // Note: But not the inverse!
    )
  )

  includes(
    "5_2",
    Set(
      "ln:B ⊑ ln:A"
    ),
    q5,
    noshapes
  )

  val q6 = gcore(
    "(x), (y)",
    "(x)",
    where = "x:A",
    set = "y:B AND x:C"
  )

  includes(
    "6_0",
    noshapes,
    q6,
    Set(
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  includes(
    "6_1",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q6,
    Set(
      "ln:A ⊑ ln:B",
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:B",
      "ln:C ⊑ ln:A"
    )
  )

  // Basically case 6_1 without the fresh variable
  val q699 = gcore(
    "(x)",
    "(x)",
    where = "x:A",
    set = "x:C"
  )

  includes(
    "6_99",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q699,
    Set(
      "ln:A ⊑ ln:B",
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:B",
      "ln:B ⊑ ln:C",
      "ln:B ⊑ ln:A",
      "ln:C ⊑ ln:A"
    )
  )

  // Basically case 6_1 without the fresh variable
  val q698 = gcore(
    "(x), (y)",
    "(x)",
    where = "x:A",
    set = "x:C"
  )

  includes(
    "6_98",
    Set(
      "ln:A ⊑ ln:B"
    ),
    q698,
    Set(
      "ln:A ⊑ ln:B",
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A",
      "ln:C ⊑ ln:B",
      "ln:B ⊑ ln:C",
      "ln:B ⊑ ln:A"
    )
  )

  includes(
    "6_2",
    Set(
      "ln:B ⊑ ln:A"
    ),
    q6,
    Set(
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  includes(
    "6_3",
    Set(
      "ln:A ⊑ ln:C"
    ),
    q6,
    Set(
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  includes(
    "6_4",
    Set(
      "ln:B ⊑ ln:C"
    ),
    q6,
    Set(
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  includes(
    "6_5",
    Set(
      "ln:C ⊑ ln:B"
    ),
    q6,
    Set(
      "ln:A ⊑ ln:C",
      "ln:C ⊑ ln:A"
    )
  )

  val q7 = gcore(
    "(x), (y), (z)",
    "(x)"
  )

  includes(
    "7_0",
    noshapes,
    q7,
    noshapes
  )

  includes(
    "7_0",
    Set(
      "ln:A ⊑ ln:C"
    ),
    q7,
    Set(
      "ln:A ⊑ ln:C"
    )
  )

  val q8 = gcore(
    "(x), (y), (z)",
    "(x)",
    set = "y:A AND z:A AND z:B"
  )

  includes(
    "8_0",
    noshapes,
    q8,
    noshapes
  )

  includes(
    "8_1",
    Set(
      "ln:B ⊑ ln:A"
    ),
    q8,
    Set(
      "ln:B ⊑ ln:A"
    )
  )

  includes(
    "8_2",
    Set(
      "ln:A ⊑ ln:C",
      "ln:A ⊑ ln:B",
      "ln:B ⊑ ln:A",
      "ln:B ⊑ ln:C",
      "ln:C ⊑ ln:A",
      "ln:C ⊑ ln:B"
    ),
    q8,
    Set(
      "ln:B ⊑ ln:A",
      "ln:C ⊑ ln:A",
      "ln:C ⊑ ln:B"
    )
  )
