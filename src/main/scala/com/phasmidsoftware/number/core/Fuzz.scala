package com.phasmidsoftware.number.core

import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering
import scala.util.Try

/**
  * Type constructor which will give fuzzy behavior to a type T.
  *
  * @tparam T the underlying type of the fuzziness. Usually Double for fuzzy numerics.
  */
trait Fuzz[T] {
  val shape: Shape

  def normalize(z: Double): Fuzz[T]

  def toString(t: T): String
}

case class RelativeFuzz[T: Valuable](tolerance: Double, shape: Shape) extends Fuzz[T] {
  /**
    * This method takes a value of T on which to base a relative fuzz value.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def absolute(t: T): Option[AbsoluteFuzz[T]] = {
    val tf = implicitly[Valuable[T]]
    Try(AbsoluteFuzz(tf.times(tf.fromDouble(tolerance), t), shape)).toOption
  }

  // TODO z is never used
  def normalize(z: Double): Fuzz[T] = shape match {
    case Gaussian => this
    case Box => RelativeFuzz(Box.toGaussianRelative(tolerance), Gaussian)
  }

  def toString(t: T): String = absolute(t).map(_.toString(t)).getOrElse("")
}

case class AbsoluteFuzz[T: Valuable](magnitude: T, shape: Shape) extends Fuzz[T] {
  /**
    * This method takes a value of T on which to base a relative fuzz value.
    * NOTE: if t is zero, we will return None, which corresponds to an Exact number.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def relative(t: T): Option[RelativeFuzz[T]] = {
    val tf = implicitly[Fractional[T]]
    Try(RelativeFuzz(tf.toDouble(tf.div(magnitude, t)), shape)).toOption
  }

  // TODO z is never used
  def normalize(z: Double): Fuzz[T] = shape match {
    case Gaussian => this
    case Box => AbsoluteFuzz(Box.toGaussianAbsolute(magnitude), Gaussian)
  }

  def zipStrings(v: String, t: String): String = {
    val q: LazyList[Char] = LazyList.from(v.toCharArray.toList) :++ LazyList.continually('0')
    val r: LazyList[Char] = q zip t map { case (a, b) => if ('b' == '0') a else b }
    new String(r.toArray)
  }

  def toString(t: T): String = {
    val tv = implicitly[Valuable[T]]
    val q = tv.toDouble(magnitude).toString.substring(2) // drop the "0."
    val (qPrefix, qSuffix) = q.toCharArray.span(_ == '0')
    val qq = new String(qPrefix) + "00" + '(' + new String(qSuffix).padTo(2, '0') + ')'
//    val zz = tv.toDouble(t)
    val z = f"${tv.toDouble(t)}%f"
    // CONSIDER why is zSuffix not used?
    val (zPrefix, zSuffix) = z.toCharArray.span(_ != '.')
    new String(zPrefix) + "." + zipStrings(new String(zPrefix).substring(1), qq)
  }

}

object Fuzz {
  /**
    * Converts all shapes into Gaussian.
    *
    * @return either RelativeFuzz or AbsoluteFuzz but always with Gaussian shape.
    */
  def normalizeFuzz[T: Valuable]: Fuzz[T] => Fuzz[T] = {
    case f@RelativeFuzz(_, Gaussian) => f
    case f@AbsoluteFuzz(_, Gaussian) => f
    case RelativeFuzz(t, _) =>
      RelativeFuzz[T](Box.toGaussianRelative(t), Gaussian)
    case AbsoluteFuzz(m, _) =>
      AbsoluteFuzz(Box.toGaussianAbsolute(m), Gaussian)
  }
}
trait Shape

case object Box extends Shape {
  /**
    * This method is to simulate a uniform distribution
    * with a normal distribution.
    *
    * @param x half the length of the basis of the uniform distribution.
    * @return x/2 which will be used as the standard deviation.
    */
  def toGaussianRelative(x: Double): Double = x / 2

  /**
    * This method is to simulate a uniform distribution
    * with a normal distribution.
    *
    * @param t the magnitude of the Box distribution.
    * @return t/2 which will be used as the standard deviation.
    */
  def toGaussianAbsolute[T: Valuable](t: T): T = implicitly[Valuable[T]].scale(t, 0.5)
}

case object Gaussian extends Shape

trait Fuzzy[T] {
  val fuzz: Option[Fuzz[T]]
}

trait Valuable[T] extends Fractional[T] {
  def fromDouble(x: Double): T

  def scale(x: T, f: Double): T
}

trait ValuableDouble extends Valuable[Double] with DoubleIsFractional with Ordering.Double.IeeeOrdering {
  def fromDouble(x: Double): Double = x

  def scale(x: Double, f: Double): Double = x * f
}

object Valuable {

  implicit object ValuableDouble extends ValuableDouble

}