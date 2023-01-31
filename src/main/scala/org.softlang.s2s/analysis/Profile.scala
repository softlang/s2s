package org.softlang.s2s.analysis

import org.softlang.s2s.core.SimpleSHACLShape
import scala.concurrent.duration.*
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import scala.util.{Try, Success, Failure}

type Profile = List[ProfileEntry]

extension (profile: Profile)
  def analyze(
      result: Either[Throwable, (Set[SimpleSHACLShape], String)],
      trialID: Int
  ): ProfileAnalysis =

    val qs = profile
      .filter(_.isInstanceOf[ProfileEntry.Problem])
      .head
      .asInstanceOf[ProfileEntry.Problem]

    val cand = profile
      .filter(_.isInstanceOf[ProfileEntry.Candidates])
      .head
      .asInstanceOf[ProfileEntry.Candidates]
      .cand

    val p = (qs.q, qs.qS, qs.sin, qs.sinS)

    if result.isLeft then FailedAnalysis(p, result.left.get, trialID)
    else
      val tstart = profile.find(_.isStart("algorithm")).get.time
      val tend = profile.find(_.isEnd("algorithm")).get.time
      val total = tstart.until(tend, ChronoUnit.MILLIS).millis

      val fstart = profile.find(_.isStart("filter")).get.time
      val fend = profile.find(_.isEnd("filter")).get.time
      val filter = fstart.until(fend, ChronoUnit.MILLIS).millis

      val lastRestart =
        profile.reverse
          .filter(_.isRestart("filter"))
          .headOption
          .map(_.time)
          .getOrElse(fstart)

      val spentRestarting = fstart.until(lastRestart, ChronoUnit.MILLIS).millis

      val timedout = profile.filter(_.isTimeout("filter")).nonEmpty

      val restarts = profile.filter(_.isRestart("filter")).size

      SuccessfulAnalysis(
        problem = p,
        result = result.right.get,
        candidates = cand.size,
        total = total,
        filtering = filter,
        timedOut = timedout,
        restartsCount = restarts,
        timeSpentRestarting = spentRestarting,
        id = trialID
      )
