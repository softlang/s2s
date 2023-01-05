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

class Profiling(config: Configuration):

  val s2s = Shapes2Shapes(config)

  private def runStep(q: SCCQ, s: Set[SimpleSHACLShape]): Long =

    println(s"Query: ${q.show(s2s.shar.state)}")
    println(s"Shapes: ${s.map(_.show(s2s.shar.state)).mkString(",")}")

    val log = Log("T", info = false, debugging = false, profiling = true)
    s2s.algorithm(q, s, log)

    val tstart = log.profile.find(_.isStart("algorithm")).get.time
    val tend = log.profile.find(_.isEnd("algorithm")).get.time
    val total = tstart.until(tend, ChronoUnit.MILLIS)

    val fstart = log.profile.find(_.isStart("filter")).get.time
    val fend = log.profile.find(_.isEnd("filter")).get.time
    val filter = fstart.until(fend, ChronoUnit.MILLIS)

    val perc = (filter.toDouble / total.toDouble) * 100

    println(s"Total: ${total}ms")
    println(f"Filtering: ${filter}ms ($perc%.2f of total)")

    total

  def run(gen: ProblemGenerator, trials: Int): Unit =

    val results = (1 to trials).map(_ =>
      val qs = gen.sample()
      runStep(qs._1, qs._2)
    )

    val avg = results.map(_ / trials.toLong).sum
    val med = results.sorted.drop(results.size / 2).headOption.getOrElse(0)
    println("Summary:")
    println(s"Executed $trials trials")
    println(s"Average execution time: ${avg}ms")
    println(s"Median execution time: ${med}ms")
    println(s"Longest trial: ${results.max}ms")
    println(s"Shortes trial: ${results.min}ms")

  def run(genConfig: ProblemGeneratorConfig, trials: Int): Unit =
    val pg = ProblemGenerator(genConfig)(s2s.scopes)
    run(ProblemGenerator(genConfig)(s2s.scopes), trials)
