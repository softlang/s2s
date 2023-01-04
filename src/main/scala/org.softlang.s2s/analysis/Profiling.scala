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

class Profiling(config: Configuration, genConfig: ProblemGeneratorConfig):

  val s2s = Shapes2Shapes(config)
  val pg = ProblemGenerator(genConfig)(s2s.scopes)

  private def runStep: Long =
    val qs = pg.sample()
    val q = qs._1
    val s = qs._2

    println(q.show(s2s.shar.state))
    s.map(_.show(s2s.shar.state)).foreach(println)

    val log = Log("T", info = false, debugging = false, profiling = true)
    s2s.algorithm(q, s, log)

    val start = log.profile.find(_.isStart("algorithm")).get.time
    val end = log.profile.find(_.isEnd("algorithm")).get.time

    start.until(end, ChronoUnit.MILLIS)

  def run(trials: Int): Unit =
    val results = (1 to trials).map(_ => runStep)
    val avg = results.map(_ / trials.toLong).sum
    println(
      "Average execution time after " ++ trials.toString ++ " trials: " ++ avg.toString ++ " milliseconds."
    )
    println("Longest trial: " ++ results.max.toString ++ " milliseconds.")
    println("Shortes trial: " ++ results.min.toString ++ " milliseconds.")
