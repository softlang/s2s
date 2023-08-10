package org.softlang.s2s.generate

import org.softlang.s2s.core._
import de.pseifer.shar.Shar
import org.softlang.s2s.parser.SCCQParser
import org.softlang.s2s.query.SCCQ

import scala.util.Failure
import scala.util.Try

/** Load queries from file (for Profiling). */
class FileLoader:

  private val shar = Shar()
  import shar._

  private val sccqp = SCCQParser(shar)
  private var it: Iterator[SCCQ] = null

  /** Iterator on queries exists. */
  def isLoaded: Boolean = it != null

  /** Load queries from a file. Drops broken queries silently. */
  def load(file: String): Unit =
    // Create buffered source from query file, where lines are queries.
    // May fail, ignored here.
    val qft = io.Source.fromFile(file).getLines
    val iti = qft.map(sccqp.parse(_)).map(_.flatMap(q => SCCQ.validate(q, "*")))

    it = for
      i <- iti
      if i.isRight
    yield i.toOption.get

  /** Get the next sample from the file iterator. */
  def getSample(): SCCQ = it.next()

end FileLoader
