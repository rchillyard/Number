package com.phasmidsoftware.number.core.misc

object Lab {

  import scala.util.Random
  // TODO a List of Double such that each element is the value of Random.nextDouble

  val n = 10000

  def xs = List.fill(n)(Random.nextDouble())

  // transform your list such that each element is between -1 and 1.
  // zip two such lists together

  def ys = xs map (_ * 2 - 1)

  val zs = ys zip ys

  // TODO define method such that it takes a (Double,Double) and returns the radius
  // that's to say math.sqrt(sum of the squares).

  def radius(x: Double, y: Double): Double = math.sqrt(x * x + y * y)

  val f = radius.tupled
  val qs: List[Double] = zs map f

  // TODO filter our list of doubles such that we retain only those with value less than 1

  val m = qs count (_ < 1)

  def main(args: Array[String]): Unit = println(4.0 * m / n)
}
