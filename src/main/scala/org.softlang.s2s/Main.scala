package org.softlang.s2s

import org.softlang.s2s.core.{Configuration, Log}
import scala.util.{Try, Failure}

/** Print reason, if a file can not be read. */
def handle[T](t: Try[T]): Unit =
  t match
    case Failure(e) => println("FAILURE " + e.getLocalizedMessage)
    case _          => ()

@main def run(queryFile: String, shapesFile: String): Unit =

  // Create buffered sources and check for errors.
  val qft = Try(io.Source.fromFile(queryFile))
  val sft = Try(io.Source.fromFile(shapesFile))

  handle(qft)
  handle(sft)

  // Run Shapes2Shapes with default config and input query/shapes.
  for
    q <- qft.map(_.getLines.mkString("\n"))
    s <- sft.map(_.getLines.toSet)
  do Shapes2Shapes(Configuration.defaultDebug).run(q, s)

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
