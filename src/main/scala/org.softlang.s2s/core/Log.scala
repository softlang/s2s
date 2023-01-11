package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.dl.Axiom
import java.time.LocalDateTime

enum ProfileEntry:
  case Start(activity: String, timeStamp: LocalDateTime)
  case End(activity: String, timeStamp: LocalDateTime)

  def isStart(activity: String): Boolean = this match
    case Start(a, _) => a == activity
    case _           => false

  def isEnd(activity: String): Boolean = this match
    case End(a, _) => a == activity
    case _         => false

  def time: LocalDateTime = this match
    case Start(_, time) => time
    case End(_, time)   => time

  override def toString(): String = this match
    case Start(a, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-start"
    case End(a, t) =>
      s"${t.toLocalTime} ${a.filter(!_.isWhitespace)}-end"

/** Log that can be printed or return as a String. Has three levels. Errors are
  * always logged.
  *   - `info`: Log some information (info method).
  *   - `debugging`: Log more information (debug method).
  *   - `profiling`: Print timestamps during execution.
  *   - `noisy`: Print all (active) logs during execution.
  */
class Log(
    topToken: String,
    info: Boolean = true,
    debugging: Boolean = false,
    profiling: Boolean = false,
    noisy: Boolean = false
):

  private var LOG: String = ""
  private var INFO: Boolean = info
  private var DEBUGGING: Boolean = debugging

  private var the_profile: List[ProfileEntry] = Nil

  /** Print log to stdout. */
  def print(
      hidecolon: Boolean = false,
      prettyVariableConcepts: Boolean = true
  ): Unit =
    val t1 =
      if prettyVariableConcepts then
        LOG
          .replaceAll(s"shar:${topToken}", s"${topToken}")
          .replaceAll("shar", "?")
      else LOG

    val t2 =
      if hidecolon then t1.replaceAll(":", "")
      else t1

    println(t2)

  /** Get log as a string. */
  override def toString: String = LOG

  // Actually append to the log. If `noisy` is set,
  // then also dump to stdout.
  private def output(s: String): Unit =
    if noisy then println(s)
    LOG += s ++ "\n\n"

  /** Log an errors. */
  def error(e: String): Unit = output(e)

  def info(s: String): Unit =
    if DEBUGGING || INFO then output(s)

  def info(title: String, s: String): Unit =
    info(title.trim ++ " = " ++ s.trim)

  def info(title: String, s: List[String]): Unit =
    info(Util.formatSet(s.toSet, prefix = title.trim ++ " = ", oneline = false))

  def debug(s: String): Unit = if DEBUGGING then info(s)

  def debug(title: String, s: String): Unit = if DEBUGGING then info(title, s)

  def debug(title: String, s: List[String]): Unit =
    if DEBUGGING then info(title, s)

  def debug(title: String, s: Set[Axiom])(implicit state: BackendState): Unit =
    debug(title, s.map(_.show).toList)

  def debugNoisy(content: String): Unit =
    if noisy then debug(content)

  def profileStart(action: String): Unit =
    val entry = ProfileEntry.Start(action, LocalDateTime.now())
    if profiling then println(entry)
    the_profile = entry :: the_profile

  def profileEnd(action: String): Unit =
    val entry = ProfileEntry.End(action, LocalDateTime.now())
    if profiling then println(entry)
    the_profile = entry :: the_profile

  def profile: List[ProfileEntry] = the_profile.reverse
