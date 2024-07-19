package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

// Test cases for simple Concept ECCQ queries.

class ConceptECCQTests extends ValidationSuite("e-concept"):

  val q0 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:A")

  // including at most the tautology ln:A ⊑ ln:A (not shown)
  includes("", noshapes, q0, noshapes)
  includes("", Set("ln:A ⊑ ln:B"), q0, noshapes)
  includes("", Set("ln:B ⊑ ln:A"), q0, noshapes)

  val q1 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:B")

  includes("", Set("ln:A ⊑ ln:B"), q1, Set("ln:A ⊑ ln:B"))
  includes("", Set("ln:B ⊑ ln:A"), q1, Set("ln:B ⊑ ln:A"))
  includes("", noshapes, q1, noshapes)

  val q2 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:B", set = "x:C")
  val s2 = Set("ln:A ⊑ ln:B", "ln:C ⊑ ln:B", "ln:A ⊑ ln:C")
  // C ⊑ A is not entailed, as y might sometimes have C;
  // B ⊑ C is not entailed as we do not now about the relationship of C and A.

  includes("", Set("ln:A ⊑ ln:B"), q2, s2)

  val q3 = gcore("(x), (y)", "(x), (y)", where = "x:A AND x:C AND y:B")
  val s3 = Set("ln:A ⊑ ln:B", "ln:C ⊑ ln:B") // further refinement on x does not matter

  includes("", Set("ln:A ⊑ ln:B"), q3, s3)

  val q4 = gcore("(x), (y)", "(x), (y)", where = "y:A AND x:C AND x:B")
  val s4 = Set("ln:C ⊑ ln:B") // but on y refinement does matter

  includes("", Set("ln:A ⊑ ln:B"), q4, s4)

  // without input shapes, we do not even know that
  includes("", noshapes, q4, noshapes)

  val q5 = gcore("(x), (y)", "(x), (y)", where = "y:A AND x:C AND x:B", set = "x:A")
  val s5 = Set("ln:C ⊑ ln:A", "ln:B ⊑ ln:A") // here we can infer that now all C and B are A's...

  includes("", noshapes, q5, s5)

  val q6 = gcore("(x), (y), (z)", "(x), (y), (z)", where = "y:A AND x:C AND x:B AND z:D", set = "x:A")
  // ...but if we add another variable, this is lost again, since we do not know about z and A...

  includes("", noshapes, q6, noshapes)
  val s6 = Set("ln:C ⊑ ln:A", "ln:B ⊑ ln:A", "ln:D ⊑ ln:A") // ...until we add sufficient knowledge.

  includes("", Set("ln:D ⊑ ln:A"), q6, s6)

  val q7 = gcore("(x)", "(x)", where = "x:A AND x:B")

  includes("", noshapes, q7, Set("ln:A ⊑ ln:B", "ln:B ⊑ ln:A"))

  val q8 = gcore("(x)", "(x)", where = "x:A AND x:B", remove = "x:A")

  includes("", noshapes, q8, noshapes) // (only the implicit tautology)

  val q9 = gcore("(x), (y)", "(x), (y)", where = "x:A AND y:D AND y:B")

  // We get B ⊑ A because only y is refined; and D ⊑ A because of the same subset relationship.
  includes("", Set("ln:B ⊑ ln:A"), q9, Set("ln:B ⊑ ln:A", "ln:D ⊑ ln:A"))

  val q10 = gcore("(x), (y), (z)", "(x), (y), (z)", where = "x:A AND y:D AND y:B AND z:C")

  // We get now only B ⊑ A, because (x) is still guaranteed to cover all A (even if z_i:A),
  // however, some z_i:D invalidate the second shape from q9.
  includes("", Set("ln:B ⊑ ln:A"), q10, Set("ln:B ⊑ ln:A"))

  val q11 = gcore("(x), (y)", "(x), (y)", where = "x:A AND x:C AND y:D AND y:B")

  // Compared to q9, we lose both shapes here, since we restrict x, not including all instances of A.
  // We do get C ⊑ A, but only because we know that Y is still a subset of X...
  includes("", Set("ln:B ⊑ ln:A"), q11, Set("ln:C ⊑ ln:A"))
  // ... if we remove this input shape, C ⊑ A no longer holds for the common reasons.
  includes("", noshapes, q11, noshapes)

  val q12 = gcore("(x), (y), (z)", "(x), (y), (z)", where = "x:A AND y:D AND y:B AND z:C", remove = "z:D")

  // Alas, if we modify example q10 such that we remove D from Z, the shape reappears.
  includes("", Set("ln:B ⊑ ln:A"), q12, Set("ln:B ⊑ ln:A", "ln:D ⊑ ln:A"), debugging = true)
