package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for minimal examples that show how concepts persist
// through G-CORE query execution.

class ConceptRetainTests extends ValidationSuite("e_ret_"):

  // Here we only have a single concept, so no shapes (except for
  // the vacouosly satisfied A ⊑ A)
  val q1 = gcore("(x)", "(x)", where = "x:A")
  includes("1_0", noshapes, q1, noshapes)

  // Except when we add input shapes.
  includes("1_1", Set("ln:A ⊑ ln:B"), q1, Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"))

  // For the inverse, we get only one, since there might be bindings of 'x'
  // which have 'A' but not 'B', but not the inverse.
  includes("1_2", Set("ln:B ⊑ ln:A"), q1,
           Set("ln:B ⊑ ln:A"),
           not = Set("ln:A ⊑ ln:B"))

  // If we add another concept, we get subsumption both with and without input shapes.
  val q2 = gcore("(x)", "(x)", where = "x:A AND x:B")

  includes("2_0", noshapes, q2, Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"))

  // We can extend this to more concepts.
  val q3 = gcore("(x)", "(x)", where = "x:A")
  includes("3_0", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:C"), q3, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A",
             "ln:A ⊑ ln:C",
             "ln:C ⊑ ln:A",
             "ln:C ⊑ ln:B",
             "ln:B ⊑ ln:C",
           ))

  // Here, we still get that everything is an A implicitly and since we have only
  // one way of constructing 'B' or 'C', we still get the initial subsumption, too.
  includes("3_1", Set("ln:B ⊑ ln:C"), q3, Set(
             "ln:B ⊑ ln:A",
             "ln:C ⊑ ln:A",
             "ln:B ⊑ ln:C",
           ), not = Set(
             "ln:A ⊑ ln:B",
             "ln:A ⊑ ln:C",
             "ln:C ⊑ ln:B",
           ))
  includes("3_2", Set("ln:C ⊑ ln:B"), q3, Set(
             "ln:B ⊑ ln:A",
             "ln:C ⊑ ln:A",
             "ln:C ⊑ ln:B",
           ), not = Set(
             "ln:A ⊑ ln:B",
             "ln:A ⊑ ln:C",
             "ln:B ⊑ ln:C",
           ))



  // SET clauses for new labels

  val q4 = gcore("(x)", "(x)", where = "x:A", set = "x:B")

  includes("4_0", noshapes, q4, Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"))
  includes("4_1", Set("ln:B ⊑ ln:C"), q4, Set(
             // From the set clause.
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A",
             // Trivially, since all things are 'A' and 'B'.
             "ln:C ⊑ ln:A",
             "ln:C ⊑ ln:B"
           ))
  includes("4_2", Set("ln:C ⊑ ln:B"), q4, Set(
             // From the set clause.
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A",
             // Trivially, since all things are 'A' and 'B'.
             "ln:C ⊑ ln:A",
             "ln:C ⊑ ln:B"
           ))
  includes("4_3", Set("ln:C ⊑ ln:D"), q4, Set(
             // From the set clause.
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A",
             // Trivially, since all things are 'A' and 'B'.
             "ln:C ⊑ ln:A",
             "ln:C ⊑ ln:B",
             "ln:D ⊑ ln:A",
             "ln:D ⊑ ln:B",
             "ln:C ⊑ ln:D"
           ))

  val q5 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:A", set = "y:B")
  includes("5_0", noshapes, q5, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A",
           ))

  val q6 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:C", set = "y:B")
  includes("6_0", noshapes, q6, Set(
             // We explicitly add 'B' to all 'C'.
             "ln:C ⊑ ln:B",
           ))

  // Compared to the previous case, we know do not know that we attach 'B' to all 'C',
  // since 'y' only matches specific 'C' and 'x' may also include 'C' that are not also 'D',
  // so not included in 'y'. This was not possible in the previous case.
  val q7 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:C AND y:D", set = "y:B")
  includes("7_0", noshapes, q7, noshapes)



  // REMOVE clauses for removing labels

  // Removal of labels - remvoing a single label means no shapes;
  // in either direction, and regardless of input shapes.
  val q8 = gcore("(x)", "(x)", where = "x:A AND x:B", remove = "x:A")
  includes("8_0", noshapes, q8, noshapes)
  includes("8_1", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"), q8, noshapes)

  // ...also in the other direction.
  val q9 = gcore("(x)", "(x)", where = "x:A AND x:B", remove = "x:B")
  includes("9_0", noshapes, q9, noshapes)

  // This also holds over additional concept (label) assertions, as we would expect.
  val q10 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:B AND y:C", remove = "y:C")

  includes("10_0", noshapes, q10, noshapes)

  includes("10_1", Set("ln:A ⊑ ln:B"), q10, Set("ln:A ⊑ ln:B"))

  includes("10_2", Set("ln:B ⊑ ln:A"), q10, Set("ln:B ⊑ ln:A"))

  includes("10_3", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"), q10, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:A"
           ))

  includes("10_4", Set("ln:A ⊑ ln:C"), q10, noshapes)
  includes("10_5", Set("ln:B ⊑ ln:C"), q10, noshapes)
  includes("10_6", Set("ln:C ⊑ ln:B"), q10, noshapes)

  // Somewhat interesting exception: We can still reason with the availability
  // of concepts before removal. Here, we know that 'C' must also be 'A', and
  // thus we know that all bindings for 'y' must also have the 'A' label.
  // As a result, B ⊑ A holds.
  includes("10_6", Set("ln:C ⊑ ln:A"), q10, Set("ln:B ⊑ ln:A"))



  // Additional Tests

  // More variables in where part.
  val q20 = gcore("(x), (z)", "(x), (y), (z)", where = "x:A AND y:B AND z:C")

  includes("20_0", noshapes, q20, noshapes)

  includes("20_1", Set("ln:A ⊑ ln:B"), q20, Set("ln:A ⊑ ln:B"))

  includes("20_2", Set("ln:B ⊑ ln:A"), q20, Set("ln:B ⊑ ln:A"))

  includes("20_3", Set("ln:B ⊑ ln:C"), q20, Set("ln:B ⊑ ln:C"))

  includes("20_4", Set("ln:C ⊑ ln:B"), q20, Set("ln:C ⊑ ln:B"))

  includes("20_5", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:C"), q20, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:C",
             "ln:A ⊑ ln:C",
           ))

  // Transitive shapes and arbitrary onstraints on both 'x' (X) and 'y' (Y).
  // Even here, shapes still hold: 'x' instances have all of 'A', 'B' and 'C'.
  // Instances of 'y' have maybe 'A' or 'B' -- but in either case, they are also
  // have 'C'.
  val q21 = gcore("(x), (y)", "(x), (y)", where = "x:A AND x:X AND y:C AND y:Y")
  includes("21_0", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:C"), q21, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:C",
             "ln:A ⊑ ln:C",
             "ln:Y ⊑ ln:C",
             "ln:X ⊑ ln:C",
           ))
