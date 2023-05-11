package org.softlang.s2s.main

import org.softlang.s2s.analysis.ConfigurationComparison
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration

/** Compare two configurations of the algorithm on structured test cases. */
object Compare:
  def run(): Unit =
    val compare = ConfigurationComparison(
      Configuration.default,
      Configuration.default
        .copy(
          // useNamespacedTop = true
        ),
      // Perform 1000 trials per generator configuration.
      trials = 10000,
      // Generate multiple results.
      stopAfterFirstResult = true,
      title1 = "Paper Configuration",
      title2 = "Paper including namespaced T"
    )

    // (1) Compare on generated test cases.
    // compare.structured()

    // (2) Compare on given input case.
    // compare.input(
    //   """
    //   |CONSTRUCT {
    //   | ?x a :A
    //   |} WHERE {
    //   | ?x a :B
    //   |}
    // """.stripMargin('|'),
    //   Set(":A ⊑ :B")
    // )
