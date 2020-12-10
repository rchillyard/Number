package com.phasmidsoftware.number.core

/**
  * Type constructor which will give fuzzy behavior to a type T.
  *
  * @tparam T the underlying type of the fuzziness. Usually Double for fuzzy numerics.
  */
trait Fuzz[T] {
  val shape: Shape
}

case class RelativeFuzz[T](tolerance: Double, shape: Shape) extends Fuzz[T] {
  //  def magnitude(t: T): T
}

case class AbsoluteFuzz[T](magnitude: T, shape: Shape) extends Fuzz[T] {
  //  def tolerance(t: T): Double
}

trait Shape

case object Box extends Shape

case object Gaussian extends Shape

trait Fuzzy[T] {
  val fuzz: Option[Fuzz[T]]
}