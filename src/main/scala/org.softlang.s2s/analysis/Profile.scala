package org.softlang.s2s.analysis

import scala.concurrent.duration.*
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import scala.util.{Try, Success, Failure}

type Profile = List[ProfileEntry]

extension (profile: Profile)
  def analyze(failure: Option[Throwable]): ProfileAnalysis =

    val qs = profile
      .filter(_.isInstanceOf[ProfileEntry.Problem])
      .head
      .asInstanceOf[ProfileEntry.Problem]

    val p = (qs.q, qs.qS, qs.sin, qs.sinS)

    if failure.isDefined then FailedAnalysis(p, failure.get)
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
        total = total,
        filtering = filter,
        timedOut = timedout,
        restartsCount = restarts,
        timeSpentRestarting = spentRestarting
      )
