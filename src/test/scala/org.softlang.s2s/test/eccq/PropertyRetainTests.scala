package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Similar to the cases ConceptRetain.
//
// Note, that candidates can not include ∃p.⊤ in their constraint;
// thus, these cases use 'entails' instead of 'includes' for now.

class PropertyRetainTests extends ValidationSuite("e_pret_"):

  val q1 = gcore("(x)", "(x)", where = "x.a")
  entails("1_0", noshapes, q1, noshapes)

  entails("1_1", Set("∃kn:a.⊤ ⊑ ∃kn:b.⊤"), q1, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤"
           ))

  entails("1_2", Set("∃kn:b.⊤ ⊑ ∃kn:a.⊤"), q1,
          Set("∃kn:b.⊤ ⊑ ∃kn:a.⊤"),
          not = Set("∃kn:a.⊤ ⊑ ∃kn:b.⊤"))

  val q2 = gcore("(x)", "(x)", where = "x.a AND x.b")

  entails("2_0", noshapes, q2, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤"
           ))

  val q3 = gcore("(x)", "(x)", where = "x.a")

  entails("3_0", Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
           ), q3, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:a.⊤ ⊑ ∃kn:c.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
           ))

  entails("3_1", Set(
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
           ), q3, Set(
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
           ), not = Set(
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:a.⊤ ⊑ ∃kn:c.⊤",
          ))

  entails("3_2", Set(
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ), q3, Set(
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ), not = Set(
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:a.⊤ ⊑ ∃kn:c.⊤",
          ))

  val q4 = gcore("(x)", "(x)", where = "x.a", set = "x.b = 0")

  entails("4_0", noshapes, q4, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
           ))

  entails("4_1", Set(
             "∃kn:b.⊤ ⊑ ∃kn:c.⊤",
           ), q4, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ))

  entails("4_2", Set(
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ), q4, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ))

  entails("4_3", Set(
             "∃kn:c.⊤ ⊑ ∃kn:d.⊤",
           ), q4, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:d.⊤ ⊑ ∃kn:a.⊤",
             "∃kn:d.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:d.⊤",
           ))


  val q5 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.a", set = "y.b = 0")
  entails("5_0", noshapes, q5, Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
           ))

  // We lack the knowledge that all 'c' must go through 'y', even if some 'x'
  // might also have the 'c' property.
  //
  // TODO Validation suggest that this case should probably hold.
  //
  // val q6 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.c", set = "y.b = 0")
  // entails("6_0", noshapes, q6, Set(
  //            "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
  //          ))

  // Compared to the previous case, we SHOULD not know that we attach 'b' to all 'c',
  // since 'y' only matches specific 'c' and 'x' may also include 'c' that are not also 'd',
  // so not included in 'y'.
  val q7 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.c AND y.d", set = "y.b = 0")
  entails("7_0", noshapes, q7, not = Set(
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:d.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:c.⊤ ⊑ ∃kn:d.⊤",
          ))

  val q8 = gcore("(x)", "(x)", where = "x.a AND x.b", remove = "x.a")
  entails("8_0", noshapes, q8, not = Set(
             // Note the negation.
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))
  entails("8_1", Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), q8, not = Set(
             // Note the negation.
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))

  val q9 = gcore("(x)", "(x)", where = "x.a AND x.b", remove = "x.b")
  entails("9_0", noshapes, q9, not = Set(
             // Note the negation.
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))
  entails("9_1", Set(
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), q9, not = Set(
             // Note the negation.
             "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
             "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))

  val q10 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.b AND y.c", remove = "y.c")

  entails("10_0", noshapes, q10, noshapes)

  entails("10_1", Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q10, Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), not = Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))

  entails("10_2", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), q10, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), not = Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  entails("10_3", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q10, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  entails("10_4", Set("∃kn:a.⊤ ⊑ ∃kn:c.⊤"), q10, not = Set("∃kn:a.⊤ ⊑ ∃kn:c.⊤"))
  entails("10_5", Set("∃kn:b.⊤ ⊑ ∃kn:c.⊤"), q10, not = Set("∃kn:b.⊤ ⊑ ∃kn:c.⊤"))
  entails("10_6", Set("∃kn:c.⊤ ⊑ ∃kn:b.⊤"), q10,not = Set("∃kn:c.⊤ ⊑ ∃kn:b.⊤"))

/*
 // TODO Port these additional test cases.

  val q11 = gcore("(x), (z)", "(x), (y), (z)", where = "x.a AND y.b AND z.c")

  entails("20_0", noshapes, q20, noshapes)

  entails("20_1", Set("ln:A ⊑ ln:B"), q20, Set("ln:A ⊑ ln:B"))

  entails("20_2", Set("ln:B ⊑ ln:A"), q20, Set("ln:B ⊑ ln:A"))

  entails("20_3", Set("ln:B ⊑ ln:C"), q20, Set("ln:B ⊑ ln:C"))

  entails("20_4", Set("ln:C ⊑ ln:B"), q20, Set("ln:C ⊑ ln:B"))

  entails("20_5", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:C"), q20, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:C",
             "ln:A ⊑ ln:C",
           ))

  // Transitive shapes and arbitrary onstraints on both 'x' (X) and 'y' (Y).
  // Even here, shapes still hold: 'x' instances have all of 'A', 'B' and 'C'.
  // Instances of 'y' have maybe 'A' or 'B' -- but in either case, they are also
  // have 'C'.
  val q21 = gcore("(x), (y)", "(x), (y)", where = "x:A AND x:X AND y:C AND y:Y")
  entails("21_0", Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:C"), q21, Set(
             "ln:A ⊑ ln:B",
             "ln:B ⊑ ln:C",
             "ln:A ⊑ ln:C",
             "ln:Y ⊑ ln:C",
             "ln:X ⊑ ln:C",
           ))
 */

  val q98 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.b AND y.c")

  entails("98_0", noshapes, q98, noshapes)

  entails("98_1", Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q98, Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), not = Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))

  entails("98_2", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), q98, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), not = Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  entails("98_3", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q98, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  val q99 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.b")

  entails("99_0", noshapes, q99, noshapes)

  entails("99_1", Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q99, Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), not = Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ))

  entails("99_2", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), q99, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
          ), not = Set(
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  entails("99_3", Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ), q99, Set(
            "∃kn:b.⊤ ⊑ ∃kn:a.⊤",
            "∃kn:a.⊤ ⊑ ∃kn:b.⊤",
          ))

  entails("99_4", Set("∃kn:a.⊤ ⊑ ∃kn:c.⊤"), q99, Set("∃kn:a.⊤ ⊑ ∃kn:c.⊤"))
  entails("99_5", Set("∃kn:b.⊤ ⊑ ∃kn:c.⊤"), q99, Set("∃kn:b.⊤ ⊑ ∃kn:c.⊤"))
  entails("99_6", Set("∃kn:c.⊤ ⊑ ∃kn:b.⊤"), q99, Set("∃kn:c.⊤ ⊑ ∃kn:b.⊤"))
