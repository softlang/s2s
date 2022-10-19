package org.softlang.s2s.core

import de.pseifer.shar.core.Showable
import de.pseifer.shar.core.BackendState

type ShassTry[T] = Either[ShassError, T]

trait ShassError:
  protected def format(tag: String, msg: String): String =
    "[Shass] " ++ tag ++ ": " ++ msg.toString
  def show(implicit state: BackendState): String

abstract class BasicShassError(tag: String, val msg: String) extends ShassError:
  def show(implicit state: BackendState): String = format(tag, msg)
  override def toString: String = format(tag, msg)

abstract class ShowableShassError(tag: String, val msg: Showable)
    extends ShassError:
  def show(implicit state: BackendState): String = format(tag, msg.show(state))
  override def toString: String = format(tag, msg.toString)

class NotAtomicError(msg: Showable)
    extends ShowableShassError("Not an atomic pattern", msg)

class UnparseableQueryError(msg: String)
    extends BasicShassError("Unparseable query", msg)

class UnsupportedQueryError(msg: Showable)
    extends ShowableShassError("Unsupported query", msg)

class UnparsableShapeError(msg: String)
    extends BasicShassError("Unparsable shape", msg)

class NotAShapeError(msg: Showable)
    extends ShowableShassError("Concept is not a shape", msg)

class NotSimpleError(msg: Showable)
    extends ShowableShassError("Not a Simple SHACL shape", msg)
