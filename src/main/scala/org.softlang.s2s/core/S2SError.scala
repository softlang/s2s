package org.softlang.s2s.core

import de.pseifer.shar.core.BackendState
import de.pseifer.shar.core.Showable

type S2STry[T] = Either[S2SError, T]

trait S2SError:
  protected def format(tag: String, msg: String, details: String = ""): String =
    "[s2s] " ++ tag ++ ": " ++ msg.toString ++ (if details != ""
                                                then " - " ++ details
                                                else "")
  def show(implicit state: BackendState): String

abstract class BasicS2SError(tag: String, val msg: String) extends S2SError:
  def show(implicit state: BackendState): String = format(tag, msg)
  override def toString: String = format(tag, msg)

class TimeoutError(timeout: Long, retry: Int)
    extends BasicS2SError(
      "Reasoning Timeout",
      s"Reasoning timed out after $timeout ms (and ${retry + 1} tries)"
    )

abstract class ShowableS2SError(
    tag: String,
    val msg: Showable,
    details: String = ""
) extends S2SError:
  def show(implicit state: BackendState): String =
    format(tag, msg.show(state), details)
  override def toString: String = format(tag, msg.toString, details)

class NotAtomicError(msg: Showable)
    extends ShowableS2SError("Not an atomic pattern", msg)

class UnparsableQueryError(msg: String)
    extends BasicS2SError("Unparsable query", msg)

class UnsupportedQueryError(msg: Showable, details: String = "")
    extends ShowableS2SError("Unsupported query", msg, details)

class UnparsableShapeError(msg: String)
    extends BasicS2SError("Unparsable shape", msg)

class NotAShapeError(msg: Showable)
    extends ShowableS2SError("Concept is not a shape", msg)

class NotSimpleError(msg: Showable)
    extends ShowableS2SError("Not a Simple SHACL shape", msg)
