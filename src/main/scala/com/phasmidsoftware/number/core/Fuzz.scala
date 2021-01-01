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

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * There are two different Fuzz[T] shapes, and they clearly have different effects.
    * When the shapes are absolute, this operation is suitable for addition of Numbers.
    * When the shapes are relative, this operation is suitable for multiplication of Numbers.
    *
    * Whether or not this is a true convolution, I'm not sure.
    * But is an operation to combine two probability density functions and, as such, f * g = g * f.
    *
    * @param convolute the convolute, which must have the same shape as this.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzz[T]): Fuzz[T]

  /**
    * Method to convert this Fuzz[T] into a Fuzz[T] with Gaussian shape.
    *
    * @param z ignored.
    * @return the equivalent Fuzz[T] with Gaussian shape.
    */
  def normalizeShape(z: Double): Fuzz[T]

  /**
    * Method to yield a String to render the given T value.
    *
    * @param t a T value.
    * @return a String which is the textual rendering of t.
    */
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


  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * This operation is suitable for multiplication of Numbers.
    *
    * @param convolute the convolute, which must have the same shape as this.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzz[T]): Fuzz[T] = if (this.shape == convolute.shape)
    convolute match {
      case RelativeFuzz(t, _) => RelativeFuzz(tolerance + t, shape)
      case _ => throw FuzzyNumberException("* operation on different styles")
    } else throw FuzzyNumberException("* operation on different shapes")

  // TODO z is never used
  def normalizeShape(z: Double): Fuzz[T] = shape match {
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

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * There are two different Fuzz[T] shapes, and they clearly have different effects.
    * This operation is suitable for addition of Numbers.
    *
    * @param convolute the convolute, which must have the same shape as this.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzz[T]): Fuzz[T] = if (this.shape == convolute.shape)
    convolute match {
      case AbsoluteFuzz(m, _) => AbsoluteFuzz(implicitly[Valuable[T]].plus(magnitude, m), shape)
      case _ => throw FuzzyNumberException("* operation on different styles")
    } else throw FuzzyNumberException("* operation on different shapes")

  // TODO z is never used
  def normalizeShape(z: Double): Fuzz[T] = shape match {
    case Gaussian => this
    case Box => AbsoluteFuzz(Box.toGaussianAbsolute(magnitude), Gaussian)
  }

  // CONSIDER why is this here?
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

  /**
    * Normalize the magnitude qualifier of the given fuzz according to relative.
    * If relative is true then the returned value will be a RelativeFuzz.
    * If relative is false then the returned value will be an AbsoluteFuzz.
    *
    * @param fuzz     the optional Fuzz to work on.
    * @param t        the value of T that the fuzz IS or result SHOULD BE relative to.
    * @param relative if true then return optional relative fuzz, else absolute fuzz.
    * @tparam T the underlying type of the Fuzz.
    * @return the optional Fuzz value which is equivalent (or identical) to fuzz, according to the value of relative.
    */
  def normalizeFuzz[T: Valuable](fuzz: Option[Fuzz[T]], t: T, relative: Boolean): Option[Fuzz[T]] =
    fuzz.flatMap(f => normalizeFuzz(t, relative, f))

  private def normalizeFuzz[T: Valuable](t: T, relative: Boolean, f: Fuzz[T]) = f match {
    case a@AbsoluteFuzz(_, _) => if (relative) a.relative(t) else Some(f)
    case r@RelativeFuzz(_, _) => if (relative) Some(f) else r.absolute(t)
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