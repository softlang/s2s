package org.softlang.s2s.main

import org.softlang.s2s.analysis.ConfigurationComparison
import org.softlang.s2s.core.ActiveReasoner
import org.softlang.s2s.core.Configuration

/** Compare two configurations of the algorithm on structured test cases. */
object Compare:
  def run(): Unit =
    // Common settings for both Configurations.
    val common = Configuration.default.copy(
      // ...
    )

    val compare = ConfigurationComparison(
      // Configuration 1:
      common.copy(
        activeReasoner = ActiveReasoner.Hermit
      ),
      // Configuration 2:
      common.copy(
        activeReasoner = ActiveReasoner.Jfact
      ),
      // Perform 1000 trials per generator configuration.
      trials = 100,
      // Generate multiple results.
      stopAfterFirstResult = false
    )

    compare.structured()
