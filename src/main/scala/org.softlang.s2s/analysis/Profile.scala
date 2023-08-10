package org.softlang.s2s.analysis

import org.softlang.s2s.core.SHACLShape
import scala.concurrent.duration.*
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import scala.util.{Try, Success, Failure}

type Profile = List[ProfileEntry]

/** Profile information, extracted from a log. */
extension (profile: Profile)
  def analyze(
      result: Either[Throwable, (Set[SHACLShape], String)],
      trialID: Int
  ): ProfileAnalysis =

    val qs = profile
      .filter(_.isInstanceOf[ProfileEntry.Problem])
      .head
      .asInstanceOf[ProfileEntry.Problem]

    val candidates = profile
      .filter(_.isInstanceOf[ProfileEntry.Candidates])
      .head
      .asInstanceOf[ProfileEntry.Candidates]
      .candidates

    val p = (qs.q, qs.qS, qs.sin, qs.sinS)

    if result.isLeft then FailedAnalysis(p, result.left.get, trialID)
    else
      val tStart = profile.find(_.isStart("algorithm")).get.time
      val tend = profile.find(_.isEnd("algorithm")).get.time
      val total = tStart.until(tend, ChronoUnit.MILLIS).millis

      val fStart = profile.find(_.isStart("filter")).get.time
      val fend = profile.find(_.isEnd("filter")).get.time
      val filter = fStart.until(fend, ChronoUnit.MILLIS).millis

      val lastRestart =
        profile.reverse
          .filter(_.isRestart("filter"))
          .headOption
          .map(_.time)
          .getOrElse(fStart)

      val spentRestarting = fStart.until(lastRestart, ChronoUnit.MILLIS).millis

      val timedOut = profile.filter(_.isTimeout("filter")).nonEmpty

      val restarts = profile.filter(_.isRestart("filter")).size

      SuccessfulAnalysis(
        problem = p,
        result = result.right.get,
        candidates = candidates.size,
        total = total,
        filtering = filter,
        timedOut = timedOut,
        restartsCount = restarts,
        timeSpentRestarting = spentRestarting,
        id = trialID
      )
