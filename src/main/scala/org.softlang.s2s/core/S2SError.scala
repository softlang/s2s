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

abstract class BasicShassError(tag: String, val msg: String) extends S2SError:
  def show(implicit state: BackendState): String = format(tag, msg)
  override def toString: String = format(tag, msg)

class TimeoutError(timeout: Long, retry: Int)
    extends BasicShassError(
      "Reasoning Timeout",
      s"Reasoning timed out after $timeout ms (and ${retry + 1} tries)"
    )

abstract class ShowableShassError(
    tag: String,
    val msg: Showable,
    details: String = ""
) extends S2SError:
  def show(implicit state: BackendState): String =
    format(tag, msg.show(state), details)
  override def toString: String = format(tag, msg.toString, details)

class NotAtomicError(msg: Showable)
    extends ShowableShassError("Not an atomic pattern", msg)

class UnparseableQueryError(msg: String)
    extends BasicShassError("Unparseable query", msg)

class UnsupportedQueryError(msg: Showable, details: String = "")
    extends ShowableShassError("Unsupported query", msg, details)

class UnparsableShapeError(msg: String)
    extends BasicShassError("Unparsable shape", msg)

class NotAShapeError(msg: Showable)
    extends ShowableShassError("Concept is not a shape", msg)

class NotSimpleError(msg: Showable)
    extends ShowableShassError("Not a Simple SHACL shape", msg)
