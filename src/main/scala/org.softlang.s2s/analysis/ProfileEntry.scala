package org.softlang.s2s.analysis

import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.query.SCCQ

import java.time.LocalDateTime
import scala.concurrent.duration.Duration

/** Entry in the performance profile. */
enum ProfileEntry(val activity: String, val time: LocalDateTime):

  // The problem.
  case Problem(
      q: SCCQ,
      sin: Set[SHACLShape],
      qS: String,
      sinS: String,
      t: LocalDateTime
  ) extends ProfileEntry("problem", t)

  case Candidates(candidates: Set[SHACLShape], t: LocalDateTime)
      extends ProfileEntry("candidates", t)

  // Start of a process.
  case Start(a: String, t: LocalDateTime) extends ProfileEntry(a, t)

  // End of a process.
  case End(a: String, t: LocalDateTime) extends ProfileEntry(a, t)

  // Timed-out event.
  case Timeout(
      a: String,
      timeout: Duration,
      afterAttempts: Int,
      t: LocalDateTime
  ) extends ProfileEntry(a, t)

  // Restart of a process event.
  case Restart(
      a: String,
      timeout: Duration,
      attempt: Int,
      maxAttempts: Int,
      t: LocalDateTime
  ) extends ProfileEntry(a, t)

  def isStart(activity: String): Boolean = this match
    case Start(a, _) => a == activity
    case _           => false

  def isEnd(activity: String): Boolean = this match
    case End(a, _) => a == activity
    case _         => false

  def isTimeout(activity: String): Boolean = this match
    case Timeout(a, _, _, _) => a == activity
    case _                   => false

  def isRestart(activity: String): Boolean = this match
    case Restart(a, _, _, _, _) => a == activity
    case _                      => false

  def isProblem: Boolean = this match
    case Problem(_, _, _, _, _) => true
    case _                      => false

  override def toString(): String = this match
    case Start(a, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-start"
    case End(a, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-end"
    case Timeout(a, to, at, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-timeout-${to.toMillis}-$at"
    case Restart(a, to, at, m, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-restart-${to.toMillis}-$at/$m"
    case Problem(_, _, q, sin, t) =>
      s"${t.toLocalTime} query-$q\n" ++
        s"${t.toLocalTime} shapes-$sin"
    case Candidates(candidates, t) =>
      s"${t.toLocalTime} candidates-${candidates.size}"
