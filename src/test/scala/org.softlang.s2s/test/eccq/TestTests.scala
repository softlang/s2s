package org.softlang.s2s.test.eccq

import org.softlang.s2s.test.ValidationSuite

class TestTests extends ValidationSuite("test_", generateValidation = false):

  // We lack the knowledge that all 'c' must go through 'y', even if some 'x'
  // might also have the 'c' property.
  //
  // Validation strongly suggests that this does indeed hold.
  val q6 = gcore("(x), (y)", "(x), (y)", where = "x.a AND y.c", set = "y.b = 0")
  entails("6_0", noshapes, q6, Set(
             "∃kn:c.⊤ ⊑ ∃kn:b.⊤",
           ), debugging = true)
