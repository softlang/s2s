package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}

@main def main: Unit =

  val common = Configuration.join(
    Configuration.debug,
    Configuration.formalOutput,
    Configuration(
      erasePvariables = false,
      eraseHvariables = false,
      approximatePvariables = false,
      approximateHvariables = false,
      closeConcepts = true,
      closeProperties = true,
      closeTop = true,
      dcaForPattern = true,
      dcaForTemplate = true,
      // cwaForPattern = false,
      cwaForTemplate = true,
      unaForPattern = false,
      unaForTemplate = true,
      optimizeCandidates = true
    )
  )

  // Compare two configurations of the algorithm.

  val compare = ConfigurationComparison(
    // Configuration 1...
    Configuration.join(
      common,
      Configuration(
        cwaForPattern = false
      )
    ),
    // ...compared vs. configuration 2.
    Configuration.join(
      common,
      Configuration(
        cwaForPattern = true
      )
    ),
    compareVariableSubsumptions = true,
    compareResults = false
  )

  val q =
    """
    CONSTRUCT {
      ?y a :B . ?x :r ?y . ?x :r ?y
    } WHERE {
      ?y :p ?y . ?x :q ?x . ?x a :A
    }
    """

  val sin = Set(
    "∃-:p.⊤ ⊑ ∃-:q.:B"
  )

  // compare.structured

  val s2s = Shapes2Shapes()

  val qu = s2s.parseQuery(q).toOption.get
  val sh = s2s.parseShapes(sin).toOption.get

  val l1 = Log()
  val l2 = Log()

  compare.compare(qu, sh, l1, l2)

  l1.print(true, true)
  l2.print(true, true)
