package org.softlang.s2s

import org.softlang.s2s.core.Configuration

// Input SCCQ q.
def q: String =
  """
  CONSTRUCT {
    ?x a :C . ?x :q ?y . ?y a :D
  } WHERE {
    ?x a :A . ?x :r ?y . ?y a :B
  }
  """

// Input set of Simple SHACL shapes S_in.
def sin: Set[String] = Set(
  ":A ⊑ ∃:r.:B",
  ":B ⊑ :A"
)

@main def main: Unit =

  // Configure Shapes2Shapes...
  val s2s = Shapes2Shapes(
    Configuration.join(
      // Using method [assumptionMethod,steffensMethod,philippsMethod]
      Configuration.philippsMethod,
      // detailed results will be printed
      Configuration.debug,
      // in (more) formal notation.
      Configuration.formalOutput
    )
  )

  // ...and run validation on the example.
  s2s.run(q, sin)
