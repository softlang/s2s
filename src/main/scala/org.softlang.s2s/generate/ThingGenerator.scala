package org.softlang.s2s.generate

import scala.util.Random
import org.antlr.v4.parse.BlockSetTransformer.elementOptions_return

/** A generator for things that have a maximum count, and a probability for
  * fresh creation and that can be generated given some integer ID.
  */
class ThingGenerator[T](
    freshProbability: Float,
    maximumCount: Int,
    generator: Int => T,
    rnd: Random
):

  override def toString: String =
    s"($freshProbability, $maximumCount) - locked:$locked - ${things}"

  // If locked, generate no fresh instances.
  private var locked: Boolean = false

  /** Flip a (weighted) coin. */
  private def flip(prop: Float = 0.5): Boolean =
    rnd.nextFloat() <= prop

  /** The generated things T in this query. */
  private var things: Set[T] = Set()

  /** Get a (possibly fresh) thing. */
  private def doSample(): T =
    if flip(freshProbability) then doFresh()
    else doSelect()

  /** Select an existing thing, unless on empty things, then just get a fresh
    * one.
    */
  private def doSelect(): T =
    if things.isEmpty then doFresh()
    else rnd.shuffle(things.toList).head

  /** Generate a fresh thing, unless maximum or locked, then get existing. */
  private def doFresh(): T =
    if things.isEmpty || ((things.size < maximumCount || maximumCount == 0) && !locked)
    then
      // Generate,
      val fresh = generator(things.size + 1)
      // add to things,
      things = things.incl(fresh)
      // and return.
      fresh
    else doSelect()

  /** Set or initialize things. */
  def setThings(t: Set[T]): Unit = things = t

  /** Guaranteed a fresh thing, unless maximum is exceeded. */
  def fresh(): T = doFresh()

  /** Sample this generator. */
  def sample(): T = doSample()

  /** Select an random, existing thing. */
  def select(): T = doSelect()

  /** Lock generation of fresh instances. */
  def lock(): Unit = locked = true

  /** Unlock generation of fresh instances. */
  def unlock(): Unit = locked = false

  /** Get all things. */
  def allThings(): Set[T] = things

  /** Reset this generator. */
  def reset(): Unit =
    locked = false
    things = Set()
