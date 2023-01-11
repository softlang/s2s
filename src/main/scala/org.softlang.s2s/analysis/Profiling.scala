package org.softlang.s2s.analysis

import de.pseifer.shar.dl.Axiom
import de.pseifer.shar.dl.Subsumption
import org.softlang.s2s.Shapes2Shapes
import org.softlang.s2s.core._
import org.softlang.s2s.generate._
import org.softlang.s2s.generate.given_Conversion_Float_ConstantFloat
import org.softlang.s2s.generate.given_Conversion_Float_Float_FloatRange
import org.softlang.s2s.generate.given_Conversion_Int_ConstantInt
import org.softlang.s2s.generate.given_Conversion_Int_Int_IntRange
import org.softlang.s2s.query.SCCQ
import org.softlang.s2s.query.vocabulary

import java.time.temporal._

class Profiling(config: Configuration, noisy: Boolean = false):

  /** Primitive progress tracker. */
  private class ProgressBar(trials: Int):
    private val header = "Trial: "
    private val ts = trials.toString.size
    private val del = List.fill(2 * ts + header.size + 1)('\b').mkString("")
    private var first: Boolean = true

    /** Update the progress bar. */
    def update(trial: Int): Unit =
      if !noisy then
        if first then first = false
        else print(del)
        print(
          header ++ trial.toString.reverse
            .padTo(ts, ' ')
            .reverse ++ "/" ++ trials.toString
        )
      // For noisy profiling, only dump the trial
      // (as much debugging info will be printed between).
      else println(s"Trial: $trial/$trials")

    /** Close the progress bar. */
    def close(): Unit =
      if !noisy then
        first = true
        print(del)

  val s2s = Shapes2Shapes(config)

  private def runStep(q: SCCQ, s: Set[SimpleSHACLShape]): (Long, Long) =

    if noisy then println(s"Query: ${q.show(s2s.shar.state)}")
    if noisy then
      println(s"Shapes: ${s.map(_.show(s2s.shar.state)).mkString(",")}")

    val log = Log(
      "T",
      info = noisy,
      debugging = noisy,
      profiling = noisy,
      noisy = noisy
    )
    s2s.algorithm(q, s, log)

    val tstart = log.profile.find(_.isStart("algorithm")).get.time
    val tend = log.profile.find(_.isEnd("algorithm")).get.time
    val total = tstart.until(tend, ChronoUnit.MILLIS)

    val fstart = log.profile.find(_.isStart("filter")).get.time
    val fend = log.profile.find(_.isEnd("filter")).get.time
    val filter = fstart.until(fend, ChronoUnit.MILLIS)

    val perc = (filter.toDouble / total.toDouble) * 100

    if noisy then println(s"Total: ${total}ms")
    if noisy then println(f"Filtering: ${filter}ms ($perc%.2f of total)")

    (total, filter)

  /** Run `trials` trials with given configuration. */
  def run(genConfig: ProblemGeneratorConfig, trials: Int): String =

    val gen = ProblemGenerator(genConfig)(s2s.scopes)

    val bar = ProgressBar(trials)

    val results = (1 to trials).map(i =>
      val qs = gen.sample()
      bar.update(i)
      runStep(qs._1, qs._2)
    )
    bar.close()

    val total = results.map(_._1)
    val filter = results.map(_._2)

    val percs = total.zip(filter).map((t, f) => f.toDouble / t.toDouble)

    val med = total.sorted.drop(total.size / 2).headOption.getOrElse(0)
    val avgperc = (percs.sum / percs.size.toDouble) * 100.0

    val report = List(
      s"Configuration: $genConfig",
      s"Trials: $trials",
      s"Average execution time: ${total.sum / total.size}ms",
      s"Median execution time: ${med}ms",
      f"Average percentage spent on filtering: $avgperc%.2f",
      s"Longest trial: ${total.max}ms",
      s"Shortes trial: ${total.min}ms"
    ).mkString("\n")

    if noisy then println(report)
    report
