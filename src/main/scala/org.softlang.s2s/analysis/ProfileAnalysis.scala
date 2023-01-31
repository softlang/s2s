package org.softlang.s2s.analysis

import org.softlang.s2s.core.SimpleSHACLShape
import org.softlang.s2s.query.SCCQ

import scala.concurrent.duration.Duration

type ProblemType = (SCCQ, String, Set[SimpleSHACLShape], String)

/** Analysis of a single-run Profile. */
sealed trait ProfileAnalysis(problem: ProblemType, id: Int):
  def csv: String

case class FailedAnalysis(
    // The problem of this run.
    problem: ProblemType,
    // The failure.
    t: Throwable,
    // Trial ID.
    id: Int
) extends ProfileAnalysis(problem, id):
  override def toString: String = s"Failed analysis: ${t.getLocalizedMessage}"

  def csv: String =
    s"$id;${t.getLocalizedMessage()};0;0;0;0;0;${problem._2};${problem._4};0;Nil"

case class SuccessfulAnalysis(
    // The problem of this run.
    problem: ProblemType,
    // Result of this analysis.
    result: (Set[SimpleSHACLShape], String),
    // Number of candidates.
    candidates: Int,
    // Total time.
    total: Duration,
    // Time spent filtering.
    filtering: Duration,
    // Did a timeout occur?
    timedOut: Boolean,
    // Number of times filtering restarted.
    restartsCount: Int,
    // Time for (non-successful) restarts.
    timeSpentRestarting: Duration,
    // Trial ID.
    id: Int
) extends ProfileAnalysis(problem, id):

  private def fmtTotal: String =
    val head = s"Total: $total"
    val tail =
      if restartsCount > 0 then s" ($totalWithoutRestats)\n"
      else "\n"
    head ++ tail

  private def fmtFilter: String =
    val head =
      f"Filtering: $filtering - ${percentageFiltering * 100.0}%.2f of total"
    val tail =
      if restartsCount > 0
      then
        f" ($filteringWithoutRestarts - ${percentageFilteringWithoutRestarts * 100.0}%.2f of total without restarts)\n"
      else "\n"
    head ++ tail

  override def toString: String =
    fmtTotal ++ fmtFilter ++
      s"Restars: $restartsCount\n" ++
      s"Timeout: $timedOut"

  /** Format as CSV value. */
  def csv: String =
    s"$id;success;${total.toMillis};${filtering.toMillis};$timedOut;$restartsCount;${timeSpentRestarting.toMillis};${problem._2};${problem._4};$candidates;${result._2}"

  /** Total time not considering restarts (only successful run). */
  def totalWithoutRestats: Duration = total - timeSpentRestarting

  /** Time spent filtering not considering restarts (only successful run). */
  def filteringWithoutRestarts: Duration = filtering - timeSpentRestarting

  /** Percentage of time spent on filtering. */
  def percentageFiltering: Double =
    filtering.length.toDouble / total.length.toDouble

  /** Percentage of time spent on filtering (no restarts). */
  def percentageFilteringWithoutRestarts: Double =
    filteringWithoutRestarts.length.toDouble
      / totalWithoutRestats.length.toDouble

object ProfileAnalysis:
  def header: String =
    "ID;Fail;Total (ms);Filtering (ms);Timeout;Restarts;Restarting (ms);Query;In;Candidates;Out"
