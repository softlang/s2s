package org.softlang.s2s.generate

import scala.util.Random

/** A generator for things that have a maximum count, and a probability for
  * fresh creation and that can be generated given some integer ID.
  */
class ThingGenerator[T](
    freshProbability: Float,
    maximumCount: Int,
    generator: Int => T
):

  // If locked, generate no fresh instances.
  private var locked: Boolean = false

  /** Flip a (weighted) coin. */
  private def flip(prop: Float = 0.5): Boolean =
    Random.nextFloat() <= prop

  /** The generated things T in this query. */
  private var things: Set[T] = Set()

  /** Get a (possibly fresh) thing. */
  private def mk: T =
    // If this is the first call, or if there are open slots and coin
    // flips for generating a fresh one (and not generator is not locked),
    // generate a new concept.
    if things.isEmpty ||
      (!locked && (things.size < maximumCount || maximumCount == -1)
        && flip(freshProbability))
    then
      // Generate,
      val fresh = generator(things.size + 1)
      // add to things,
      things = things.incl(fresh)
      // and return.
      fresh
    // Else, draw a random existing one.
    else Random.shuffle(things).head

  /** Sample this generator. */
  def draw(): T = mk

  /** Lock generation of fresh instances. */
  def lock(): Unit = locked = true

  /** Unlock generation of fresh instances. */
  def unlock(): Unit = locked = false

  /** Reset this generator. */
  def reset(): Unit =
    locked = false
    things = Set()
