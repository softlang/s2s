package org.softlang.s2s.analysis

import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.dl.Subsumption
import org.softlang.s2s.infer.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.vocabulary

import scala.util.{Try, Success, Failure}

import java.time.LocalDateTime
import java.nio.file.{Paths, Files}
import java.nio.file.StandardOpenOption
import java.nio.charset.StandardCharsets

class Profiling(
    config: Configuration,
    noisy: Boolean,
    logTime: Boolean = false,
    logNoisy: Boolean = false
):

  /** Primitive progress tracker. */
  private class ProgressBar(trials: Int):
    private val header = "Trial: "
    private val ts = trials.toString.size
    private val del = List.fill(2 * ts + header.size + 1)('\b').mkString("")
    private var first: Boolean = true

    // If any output occurs, do not print progress.
    private val anyOut = noisy || logTime || logNoisy

    /** Update the progress bar. */
    def update(trial: Int): Unit =
      if !anyOut then
        if first then first = false
        else print(del)
        print(
          header ++ trial.toString.reverse
            .padTo(ts, ' ')
            .reverse ++ "/" ++ trials.toString
        )
      // For anyOut + noisy profiling, only dump the trial
      // (as much debugging info will be printed between).
      else if noisy then println(s"Trial: $trial/$trials")
      // Otherwise, do not print anything.

    /** Close the progress bar. */
    def close(): Unit =
      if !anyOut then first = true

  private def runStep(
      q: SCCQ,
      s: Set[SHACLShape],
      trialID: Int
  ): ProfileAnalysis =

    val s2s = Shapes2Shapes(config)

    // Format problem.

    val qS = q.show(s2s.shar.state)
    val sinS = s.map(_.show(s2s.shar.state)).mkString(",")

    if noisy then println(s"Query: $qS")
    if noisy then println(s"Shapes: $sinS")

    // Create a log and run algorithm on that log.

    val log = Log(
      info = true,
      debugging = true,
      profiling = logTime,
      noisy = logNoisy
    )

    log.problem(q, s, qS, sinS)

    val analysis =
      try {
        val t = Try(s2s.algorithm(q, s, log))
        // Create performance profile analysis.
        log.profile.analyze(
          t.toEither.map(r =>
            (
              r.toOption.getOrElse(Set()),
              r.toOption
                .getOrElse(Set())
                .map(_.show(s2s.shar.state))
                .mkString(",")
            )
          ),
          trialID
        )

      } catch {
        case e: OutOfMemoryError =>
          println("Warning: OutOfMemoryError. Run might be broken.")
          log.profile.analyze(Left(e), trialID)
        case e =>
          log.profile.analyze(Left(e), trialID)
      }

    if noisy then println(analysis.toString ++ "\n")

    analysis

  /** Run `trials` trials with given configuration. */
  def run(
      // Generator configuration.
      genConfig: ProblemGeneratorConfig,
      // Number of trials.
      setTrials: Int,
      // Write CSV to file.
      writeToFile: Boolean = true,
      // Number of chunks (saves intermediate results).
      chunkCount: Int = 10,
      // Run additional and drop results for the first X samples.
      dropFirstX: Int = 10,
      // Repeat trials.
      repeatTrials: Int = 1
  ): Unit =

    object Sampler:
      val gen = ProblemGenerator(genConfig)(Shapes2Shapes(config).scopes)
      var cachedSample = gen.sample()
      var counter = 0
      def sample(repeat: Int): (SCCQ, Set[SHACLShape]) =
        if counter == repeat then
          counter = 0
          cachedSample = gen.sample()
        counter += 1
        cachedSample

    // IO Files.

    val time = LocalDateTime.now()
    val timeFmt = time.toString.replaceAll(":", "_")
    val runFile = s"profile_${timeFmt}.csv"
    val metaFile = s"profile_${timeFmt}.meta.txt"

    // Execute all trials.

    val trials = repeatTrials * setTrials
    val allTrials = trials + dropFirstX

    val bar = ProgressBar(allTrials)

    var trial = 0
    var results: List[ProfileAnalysis] = Nil
    val chunkSize = trials / chunkCount

    // val drop1 = gen.sample()
    // val drop2 = gen.sample()

    // Run all trials in chunks.
    while (trial < allTrials)
      var chunk = 0

      // Process a single chunk.
      while (chunk < chunkSize && trial < allTrials)
        chunk += 1
        trial += 1

        val qs = Sampler.sample(repeatTrials)
        bar.update(trial)
        results = results ++ List(runStep(qs._1, qs._2.toList.toSet, trial))

      val actualResults = results.drop(dropFirstX)

      // Create intermediate report at the end of chunks.
      val report = actualResults.evaluate(trials, config.retry, config.timeout)

      val meta = List(
        s"Time: ${time}",
        s"Trials: ${trials}",
        s"Reasoner: ${config.reasoner}",
        s"Max Retries: ${config.retry}",
        s"Timeout: ${config.timeout}",
        s"--Configuration--\n${genConfig.formatLong}",
        s"--Report--\n$report"
      ).mkString("\n")

      if writeToFile then
        Files.write(
          Paths.get(runFile),
          actualResults.csv.getBytes(StandardCharsets.UTF_8)
        )

        Files.write(
          Paths.get(metaFile),
          meta.getBytes(StandardCharsets.UTF_8)
        )

    bar.close()
