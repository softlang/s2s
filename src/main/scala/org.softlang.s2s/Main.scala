package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}

@main def main: Unit =

  val common = Configuration.join(
    // Enable debugging.
    Configuration.debug,
    // Enable more formal output.
    Configuration.formalOutput,
    // Standard settings.
    Configuration(
      erasePvariables = false,
      eraseHvariables = false,
      approximatePvariables = false,
      approximateHvariables = false,
      closeConcepts = true,
      closeProperties = true,
      closeTop = false,
      dcaForPattern = true,
      dcaForTemplate = false,
      //cwaForPattern = false,
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
      // Common settings.
      common,
      // Custom settings for Configuration 1.
      Configuration(
        cwaForPattern = false
      )
    ),
    // ...compared vs. configuration 2.
    Configuration.join(
      // Common settings.
      common,
      // Custom settings for Configuration 2.
      Configuration(
        cwaForPattern = true
      )
    ),
    // Compare S_out for any differences.
    compareResults = true,
    // Compare all subsumptions between query variables as well.
    compareVariableSubsumptions = true,
    // 1000 steps per trial.
    stepTrials = 1000,
    // No random trials.
    randomSets = 0,
    // Generate multiple results.
    stopAfterFirstResult = false
  )

  // Run structured comparison test.
  //compare.structured()

  // Single test case.
  val example1 = (
    """
      CONSTRUCT {
        ?x a :B . ?y a :C
      } WHERE {
        ?x :p ?x . ?y a :A
      }
      """,
      Set(
        ":A ⊑ ∃:p.:A"
      ))

  // Run comparison for a single case.
  compare.standalone(example1._1, example1._2)
