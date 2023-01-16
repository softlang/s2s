package org.softlang.s2s.analysis

import scala.concurrent.duration.*

/** Multiple ProfileAnalysis. */
type ProfileAnalyses = List[ProfileAnalysis]

case class TriValue[T](v: T, wr: T, nt: T)

case class ProfileAnalysesEvaluation(
    trials: Int,
    failureCount: Int,
    failures: List[FailedAnalysis],
    samples: TriValue[Int],
    averageExecutionTime: TriValue[Duration],
    medianExecutionTime: TriValue[Duration],
    averagePercentageFiltering: TriValue[Double],
    longest: TriValue[Duration],
    shortest: TriValue[Duration]
):

  private def fmtTri[T](label: String, v: TriValue[T]): String =
    List(
      s"$label: ${v.v}",
      s"  without restarts: ${v.wr}",
      s"  no timeouts: ${v.nt}"
    ).mkString("\n")

  private def fmtTrd(label: String, v: TriValue[Double]): String =
    List(
      f"$label: ${v.v * 100.0}%.2f",
      f"  without restarts: ${v.wr * 100.0}%.2f",
      f"  no timeouts: ${v.nt * 100.0}%.2f"
    ).mkString("\n")

  private def fmtSim[T](label: String, v: T): String = s"$label: $v"

  private def fmtLst[T](label: String, lst: List[T]): String =
    if lst.isEmpty then s"$label: -\n"
    else s"$label:\n" ++ lst.map("  " ++ _.toString).mkString("\n")

  override def toString: String = List(
    fmtSim("Trials", trials),
    fmtSim("Failures", failureCount),
    fmtTri("Samples", samples),
    fmtTri("Average execution time", averageExecutionTime),
    fmtTri("Median execution time", medianExecutionTime),
    fmtTrd("Average percentage spent on filtering", averagePercentageFiltering),
    fmtTri("Longest sample", longest),
    fmtTri("Shortest sample", shortest),
    fmtLst("Failures", failures)
  ).mkString("\n")

extension (inAnalyses: ProfileAnalyses)

  /** Format at CSV. */
  def csv: String =
    (ProfileAnalysis.header :: inAnalyses.map(_.csv)).mkString("\n")

  /** Join two analyses. */
  def join(other: ProfileAnalyses): ProfileAnalyses =
    inAnalyses ++ other

  /** Summarize a report over multiple ProfileAnalysis. */
  def evaluate(
      trials: Int,
      maxRestarts: Int,
      timeoutL: Long
  ): ProfileAnalysesEvaluation =

    val analyses = inAnalyses.flatMap(a =>
      a match
        case s: SuccessfulAnalysis => List(s)
        case _                     => Nil
    )

    val failures = inAnalyses.flatMap(a =>
      a match
        case f: FailedAnalysis => List(f)
        case _                 => Nil
    )

    val timeout = timeoutL.millis
    val size = analyses.size

    val totals = analyses.map(_.total)
    val totalsWR = analyses.map(_.totalWithoutRestats)
    val totalsNT = analyses.filter(!_.timedOut).map(_.total)

    val filterings = analyses.map(_.filtering)
    val filteringsWR = analyses.map(_.filteringWithoutRestarts)
    val filteringsNT = analyses.filter(!_.timedOut).map(_.filtering)

    val percs = analyses.map(_.percentageFiltering)
    val percsWR = analyses.map(_.percentageFilteringWithoutRestarts)
    val percsNT = analyses.filter(!_.timedOut).map(_.percentageFiltering)

    val restarts = analyses.map(_.restartsCount)
    val timeouts = analyses.map(_.timedOut)

    ProfileAnalysesEvaluation(
      trials,
      failures.size,
      failures = failures,
      samples = TriValue(
        totals.size,
        totalsWR.size,
        totalsNT.size
      ),
      averageExecutionTime = TriValue(
        ProfileAnalyses.averageDuration(totals),
        ProfileAnalyses.averageDuration(totalsWR),
        ProfileAnalyses.averageDuration(totalsNT)
      ),
      medianExecutionTime = TriValue(
        ProfileAnalyses.medianDuration(totals),
        ProfileAnalyses.medianDuration(totalsWR),
        ProfileAnalyses.medianDuration(totalsNT)
      ),
      averagePercentageFiltering = TriValue(
        percs.sum / percs.size,
        percsWR.sum / percsWR.size,
        percsNT.sum / percsNT.size
      ),
      longest = TriValue(
        totals.max,
        totalsWR.max,
        totalsNT.max
      ),
      shortest = TriValue(
        totals.min,
        totalsWR.min,
        totalsNT.min
      )
    )

object ProfileAnalyses:
  def averageDuration(vs: List[Duration]): Duration =
    if vs.isEmpty then 0.millis
    else (vs.fold(0.millis)(_ + _) / vs.size).toMillis.millis

  def medianDuration(vs: List[Duration]): Duration =
    vs.sorted.drop(vs.size / 2).headOption.getOrElse(0.millis)
