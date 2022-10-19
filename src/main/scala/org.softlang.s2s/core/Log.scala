package org.softlang.s2s.core

/** Log that can be printed or return as a String. Has three levels. Errors are
  * always logged.
  *   - `info`: Log some information (`put`).
  *   - `debugging`: Log more information (`debug`).
  */
class Log(info: Boolean = true, debugging: Boolean = false):

  private var LOG: String = ""
  private var INFO: Boolean = info
  private var DEBUGGING: Boolean = debugging

  /** Print log to stdout. */
  def print(hidecolon: Boolean = false): Unit =
    if hidecolon then
      println(LOG.replaceAllLiterally(":", "").replaceAllLiterally("shar", "?"))
    else println(LOG)

  /** Get log as a string. */
  override def toString: String = LOG

  private def output(s: String): Unit =
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
