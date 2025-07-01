package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple Concept ECCQ queries.

class PropertyECCQTests extends ValidationSuite("e_props_"):

  // Here, we remove name, so not enough information to infer any shapes.

  val q0 = gcore("(x)", "(x)", where = "x:A AND x.name", remove = "x.name")
  includes("0_0", noshapes, q0, noshapes)

  // With a concrete variable set, this suffices -- as we do not remove age.
  // These shapes are included only, because we're dealing with a single variable,
  // so the environment is fully controlled.

  val q1 = gcore("(x)", "(x)", where = "x:A AND x.age = 42")
  val s1out = Set(
    // Because all with prop age have label A.
    "∃kn:age.⊤ ⊑ ln:A",
    // Because all with incoming age edge (the properties 'age') have such an
    // edge from something labelled with A.
    // (Can this have any meaning in ProGS though? Only if we could target properties in some way, I think.
    // so not in the simple subset of ProGS?)
    "∃-kn:age.⊤ ⊑ ∃-kn:age.ln:A",
    // Why? Because the rhs is generally true in this instance (?)
    "∃kn:age.⊤ ⊑ ∀-kn:age.ln:A"
  )
  includes("1_0", noshapes, q1, s1out)

  // No shapes, since we do not know about y (could be age, could be A).
  // That is, similarly to case q1, but now we can not make assumptions about age and A.

  val q2 = gcore("(x), (y)", "(x), (y)", where = "x.age = 42 AND x:A AND y:B")
  includes("2_0", noshapes, q2, noshapes)

  // If we remove A and age from y explicitly, we get the shapes as in q2.
  // Now, we can make assumptions about age and A.

  val q3 = gcore(
    "(x), (y)",
    "(x), (y)",
    where = "x.age = 42 AND x:A AND y:B",
    remove = "y.age AND y:A"
  )
  includes("3_0", noshapes, q3, s1out)

  // Does not help to add random labels or properties. Only for B and F we can infer subsumption,
  // as B is unconstrained in the WHERE clause.

  val q4 = gcore(
    "(x), (y)",
    "(x), (y)",
    where = "x.age = 42 AND x:A AND y:B",
    set = "x:E AND y:F AND x.num = 19 AND y.nnn = 0"
  )
  includes("4_0", noshapes, q4, Set("ln:B ⊑ ln:F"))

  val q5 = gcore("(x)", "(x)", where = "x:A AND x.name")
  val s5out = Set(
    // See q1.
    "∃kn:name.⊤ ⊑ ln:A",
    "∃-kn:name.⊤ ⊑ ∃-kn:name.ln:A",
    // Not entirely sure why; see q1.
    "∃kn:name.⊤ ⊑ ∀-kn:name.ln:A"
  )
  includes("5_0", noshapes, q5, s5out)
