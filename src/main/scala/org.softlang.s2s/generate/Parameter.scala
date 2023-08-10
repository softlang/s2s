package org.softlang.s2s.generate

import scala.util.Random

/** A parameter for random sampling. */
sealed trait Parameter[T: Numeric]:
  def sample(rnd: Random): T
  def min: T
  def max: T

/** An parameter for Int values. */
trait IntParameter extends Parameter[Int]:
  def mult(j: Float): Unit

/** An parameter for Int values, that are constant. */
case class ConstantInt(private var i: Int) extends IntParameter:
  override def toString: String = i.toString
  def sample(rnd: Random): Int = i
  def mult(j: Float): Unit =
    i = (i.toFloat * j).round

  def min: Int = i
  def max: Int = i

given Conversion[Int, ConstantInt] with
  def apply(i: Int): ConstantInt = ConstantInt(i)

/** An parameter for Int values, on a range. */
case class IntRange(private var mini: Int, private var maxi: Int)
    extends IntParameter:
  override def toString: String = min.toString ++ "-" ++ max.toString
  def sample(rnd: Random): Int =
    rnd.between(min, max)
  def mult(j: Float): Unit =
    mini = (min.toFloat * j).round
    maxi = (max.toFloat * j).round

  def min: Int = mini
  def max: Int = maxi

given Conversion[(Int, Int), IntRange] with
  def apply(i: (Int, Int)): IntRange = IntRange(i._1, i._2)

/** An parameter for Float values. */
trait FloatParameter extends Parameter[Float]

/** An parameter for Float values, that are constant. */
case class ConstantFloat(f: Float) extends FloatParameter:
  override def toString: String = f.toString
  def sample(rnd: Random): Float = f
  def min: Float = f
  def max: Float = f

given Conversion[Float, ConstantFloat] with
  def apply(i: Float): ConstantFloat = ConstantFloat(i)

/** An parameter for Float values, on a range. */
case class FloatRange(private var minF: Float, private var maxF: Float)
    extends FloatParameter:
  override def toString: String = min.toString ++ "-" ++ max.toString
  def sample(rnd: Random): Float =
    rnd.between(min, max)
  def min: Float = minF
  def max: Float = maxF

given Conversion[(Float, Float), FloatRange] with
  def apply(i: (Float, Float)): FloatRange = FloatRange(i._1, i._2)
