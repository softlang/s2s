package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}

@main def run(queryFile: String, shapesFile: String): Unit =

  // Load query and shapes from source.
  val qf = scala.io.Source.fromFile(queryFile)
  val sf = scala.io.Source.fromFile(shapesFile)

  // Configure Shapes 2 Shapes.
  val s2s = Shapes2Shapes(
    Configuration.join(
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
        useSubsumptionInPatternDCA = false,
        useSubsumptionInTemplateDCA = true,
        closeConcepts = true,
        closeProperties = true,
        closeTop = false,
        closeLiterals = false,
        useSubsumptionInPatternCWA = true,
        useSubsumptionInTemplateCWA = false,
        dcaForPattern = true,
        dcaForTemplate = true,
        cwaForPattern = true,
        cwaForTemplate = true,
        unaForPattern = false,
        unaForTemplate = true,
        optimizeCandidates = true
      )
    )
  )

  // Run with the example.
  s2s.run(qf.getLines.mkString("\n"), sf.getLines.toSet)

// Compare two configurations of the algorithm.
def compare(): Unit =

  // Common settings for both Configurations.
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
      useSubsumptionInPatternDCA = false,
      useSubsumptionInTemplateDCA = true,
      closeConcepts = true,
      closeProperties = true,
      closeTop = false,
      dcaForPattern = true,
      dcaForTemplate = true,
      // cwaForPattern = false,
      cwaForTemplate = true,
      unaForPattern = false,
      unaForTemplate = true,
      optimizeCandidates = true
    )
  )

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

  // (Option 1) Run structured comparison test.
  compare.structured()

  // (Option 2) Run comparison for a single case.

  // val example1 = (
  //  """
  //    CONSTRUCT {
  //      ?x a :B . ?y a :C
  //    } WHERE {
  //      ?x :p ?x . ?y a :A
  //    }
  //    """,
  //  Set(
  //    ":A ⊑ ∃:p.:A"
  //  )
  // )

  // compare.standalone(example1._1, example1._2)
