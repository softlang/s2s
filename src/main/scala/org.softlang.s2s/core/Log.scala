package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.dl.Axiom

import org.softlang.s2s.analysis.Profile
import org.softlang.s2s.analysis.ProfileEntry
import org.softlang.s2s.core.SHACLShape
import org.softlang.s2s.core.Scopes
import org.softlang.s2s.query.SCCQ

import java.time.LocalDateTime
import scala.concurrent.duration.Duration

/** Log that can be printed or return as a String. Has three levels. Errors are
  * always logged.
  *   - `info`: Log some information (info method).
  *   - `debugging`: Log more information (debug method).
  *   - `profiling`: Print timestamps during execution.
  *   - `noisy`: Print all (active) logs during execution.
  */
class Log(
    info: Boolean = true,
    debugging: Boolean = false,
    profiling: Boolean = false,
    noisy: Boolean = false
)(implicit val scopes: Scopes):

  // Internal state, the log and the profiling data.
  private var LOG: String = ""
  private var PROFILE: Profile = Nil

  // Actually append to the log. If `noisy` is set,
  // then also dump to stdout.
  private def output(s: String): Unit =
    if noisy then println(s)
    LOG += s ++ "\n\n"

  /** Append another log to this log. */
  def append(ol: Log): Unit =
    LOG = LOG ++ ol.LOG
    PROFILE = PROFILE ++ ol.PROFILE

  // Public API for formatting through output(s: String)

  /** Force some (error) message to the log. */
  def error(e: String): Unit = output(e)

  /** Log information. */
  def info(s: String): Unit =
    if debugging || info then output(s)

  /** Log information with label. */
  def info(title: String, s: String): Unit =
    info(title.trim ++ " = " ++ s.trim)

  /** Log list of information with label. */
  def info(title: String, s: List[String], sorted: Boolean = false): Unit =
    info(
      Util.formatSet(
        s.toSet,
        prefix = title.trim ++ " = ",
        oneLine = false,
        sorted = sorted
      )
    )

  /** Log set of information with label. */
  def info(title: String, s: Set[String]): Unit =
    info(title, s.toList, sorted = true)

  /** Log information (in debugging mode). */
  def debug(s: String): Unit = if debugging then info(s)

  /** Log information (in debugging mode) with label. */
  def debug(title: String, s: String): Unit = if debugging then info(title, s)

  /** Log list of information (in debugging mode) with label. */
  def debug(title: String, s: List[String], sorted: Boolean): Unit =
    if debugging then info(title, s, sorted)

  /** Log set of axioms (in debugging mode) with label. */
  def debug(title: String, s: Set[Axiom])(implicit
      state: BackendState
  ): Unit =
    debug(title, s.map(_.show).toList, sorted = true)

  /** Log set of axioms (in debugging mode) with label. */
  def debug(title: String, s: Set[String]): Unit =
    debug(title, s.toList, sorted = true)

  /** Debug information only if in noisy mode. */
  def debugNoisy(content: String): Unit =
    if noisy then debug(content)

  /** Start profiling of an action with a label. */
  def profileStart(action: String): Unit =
    val entry = ProfileEntry.Start(action, LocalDateTime.now())
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  /** End profiling of an action with a label (must be started first). */
  def profileEnd(action: String): Unit =
    val entry = ProfileEntry.End(action, LocalDateTime.now())
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  /** Log restarting of a task. */
  def restart(
      action: String,
      attempt: Int,
      maxAttempts: Int,
      timeout: Duration
  ): Unit =
    val entry =
      ProfileEntry.Restart(
        action,
        timeout,
        attempt,
        maxAttempts,
        LocalDateTime.now()
      )
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  /** Log final timeout of a task. */
  def timeout(action: String, attempts: Int, timeout: Duration): Unit =
    val entry =
      ProfileEntry.Timeout(
        action,
        timeout,
        attempts,
        LocalDateTime.now()
      )
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  def problem(
      q: SCCQ,
      sin: Set[SHACLShape],
      qS: String,
      sinS: String
  ): Unit =
    val entry =
      ProfileEntry.Problem(q, sin, qS, sinS, LocalDateTime.now())
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  def candidates(c: Set[SHACLShape]): Unit =
    val entry =
      ProfileEntry.Candidates(c, LocalDateTime.now())
    if profiling then println(entry)
    PROFILE = entry :: PROFILE

  def profile: List[ProfileEntry] = PROFILE.reverse

  // IO/Formatting

  /** Print this log to stdout. */
  def print(
      hidecolon: Boolean = false,
      prettyVariableConcepts: Boolean = true,
      prettyScopes: Boolean = true
  ): Unit =
    val t1 =
      if prettyVariableConcepts then LOG.replaceAll("shar", "?")
      else LOG

    val t2 =
      if hidecolon then t1.replaceAll(":", "")
      else t1

    val t3 =
      if prettyScopes then scopes.prettyScopeTokens(t2)
      else t2

    println(t3)

  /** Get log as a string. */
  override def toString: String = LOG
