package org.softlang.s2s.generate

import scala.util.Random

sealed trait Parameter[T: Numeric]:
  def sample: T

trait IntParameter extends Parameter[Int]:
  def mult(j: Float): Unit

case class ConstantInt(private var i: Int) extends IntParameter:
  override def toString: String = i.toString
  def sample: Int = i
  def mult(j: Float): Unit =
    i = (i.toFloat * j).round

given Conversion[Int, ConstantInt] with
  def apply(i: Int): ConstantInt = ConstantInt(i)

case class IntRange(private var min: Int, private var max: Int)
    extends IntParameter:
  override def toString: String = min.toString ++ "-" ++ max.toString
  def sample: Int = Random.between(min, max)
  def mult(j: Float): Unit =
    min = (min.toFloat * j).round
    max = (max.toFloat * j).round

given Conversion[(Int, Int), IntRange] with
  def apply(i: (Int, Int)): IntRange = IntRange(i._1, i._2)

trait FloatParameter extends Parameter[Float]

case class ConstantFloat(f: Float) extends FloatParameter:
  override def toString: String = f.toString
  def sample: Float = f

given Conversion[Float, ConstantFloat] with
  def apply(i: Float): ConstantFloat = ConstantFloat(i)

case class FloatRange(min: Float, max: Float) extends FloatParameter:
  override def toString: String = min.toString ++ "-" ++ max.toString
  def sample: Float = Random.between(min, max)

given Conversion[(Float, Float), FloatRange] with
  def apply(i: (Float, Float)): FloatRange = FloatRange(i._1, i._2)
