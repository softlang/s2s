package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

class TestTests extends ValidationSuite("test_", generateValidation = false):

  // Query from the running example in the paper.
  val paper_query =
    gcore(
      construct = "(x)-[e]->(y)",
      matc = "(x)-[e]->(y)",
      where = "y:Person AND e:observes AND x.name = \"Smith\"",
      set = "y:POI",
      remove = "y:Person"
    )

  // Shapes from the running example in the paper
  // -- explicit encoding mentioned in the paper.
  val paper_shapes_in = Set(
    "ln:Agent ⊑ #E :meta_nte.(le:observes ⊓ #E :meta_etn.ln:Person)",
    "le:observes ⊑ (#E -:meta_nte.ln:Agent) ⊓ (#E :meta_etn.ln:Person)"
  )
  // :Agent ⊑ ∃:meta_nte.(:observes)⊓(∃:meta_etn.:Person)
  // :observes ⊑ (∃-:meta_nte.:Agent)⊓(∃:meta_etn.:Person)

  // A different encoding of the same set of shapes (closer to ProGS).
  val paper_shapes_in_alternate = Set(
    "ln:Agent ⊑ ->E (le:observes ⊓ ⇒ ln:Person)",
    "le:observes ⊑ (⇐ ln:Agent) ⊓ (⇒ ln:Person)"
  )

  // The output shapes mentioned in the running example.
  val paper_shapes_out = Set(
    "le:observes ⊑ ⇒ ln:POI", // from query itself
    "le:observes ⊑ ⇐ ln:Agent", // from input shapes
    "ln:Agent ⊑ ->E le:observes" // from input shapes
  )

  // Prove that these are entailed.
  entails(
    "paper",
    paper_shapes_in,
    paper_query,
    paper_shapes_out,
    debugging = true
  )

  // Prove that these are entailed.
  entails(
    "paper_alt",
    paper_shapes_in_alternate,
    paper_query,
    paper_shapes_out,
    debugging = true
  )

  // An extended set of shapes (complete set with the default candidate generator).
  val paper_shapes_ext = Set(
    "∃-kn:name.⊤ ⊑ ∀:meta_nte.le:observes",
    "∃kn:name.⊤ ⊑ ∀-:meta_etn.le:observes",
    "∃-kn:name.⊤ ⊑ ∀:meta_etn.ln:POI"
  )

  // The union of the out and ext shapes are all included in enumerated output.
  includes(
    "paper_full",
    paper_shapes_in,
    paper_query,
    paper_shapes_out.union(paper_shapes_ext)
  )

  // The union of the out and ext shapes are all included in enumerated output.
  includes(
    "paper_full_alt",
    paper_shapes_in_alternate,
    paper_query,
    paper_shapes_out.union(paper_shapes_ext)
  )
