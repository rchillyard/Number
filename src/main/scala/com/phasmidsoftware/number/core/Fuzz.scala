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
  /**
    * One of two shapes for the probability density function:
    * Gaussian (normal distribution);
    * Box (uniform distribution).
    */
  val shape: Shape

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return an (optional) transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzz[T]

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
    * Method to possibly change the style of this Fuzz[T}.
    *
    * @param t        the magnitude of the relevant Number.
    * @param relative if true then change to Relative (if Absolute).
    * @return the (optional) Fuzz as a Relative or Absolute Fuzz, according to relative.
    */
  def normalize(t: T, relative: Boolean): Option[Fuzz[T]]

  /**
    * Method to convert this Fuzz[T] into a Fuzz[T] with Gaussian shape.
    *
    * @return the equivalent Fuzz[T] with Gaussian shape.
    */
  def normalizeShape: Fuzz[T]

  /**
    * Method to yield a String to render the given T value.
    *
    * @param t a T value.
    * @return a String which is the textual rendering of t with this Fuzz applied.
    */
  def toString(t: T): String

  /**
    * Yields the actual probability density for the value t.
    *
    * @param t a particular T value, for which we want the probability density.
    * @return the probability density.
    */
  def probabilityDensity(t: T): Double

  /**
    * Determine the range +- t within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @return the value of t at which the probability density is exactly transitions from likely to not likely.
    */
  def likely: T
}

case class RelativeFuzz[T: Valuable](tolerance: Double, shape: Shape) extends Fuzz[T] {

  private val tv: Valuable[T] = implicitly[Valuable[T]]

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return a transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzz[T] = RelativeFuzz(tv.toDouble(func(tv.fromDouble(tolerance))), shape)

  /**
    * This method takes a value of T on which to base a relative fuzz value.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def absolute(t: T): Option[AbsoluteFuzz[T]] = {
    val tf = tv
    Try(AbsoluteFuzz(tf.normalize(tf.times(tf.fromDouble(tolerance), t)), shape)).toOption
  }

  /**
    * Return either a AbsoluteFuzz equivalent or this, according to relative.
    *
    * @param t        the magnitude of the relevant Number.
    * @param relative if true then convert to RelativeFuzz otherwise wrap this in Some().
    * @return the (optional) Fuzz as a Relative or Absolute Fuzz, according to relative.
    */
  def normalize(t: T, relative: Boolean): Option[Fuzz[T]] = if (relative) Some(this) else absolute(t)

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * This operation is suitable for multiplication of Numbers.
    *
    * @param convolute the convolute, which must have the same shape as this.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzz[T]): Fuzz[T] = if (this.shape == convolute.shape)
    convolute match {
      case RelativeFuzz(t, Gaussian) => RelativeFuzz(Gaussian.convolutionProduct(tolerance, t), shape)
      case _ => throw FuzzyNumberException("* operation on different styles")
    } else throw FuzzyNumberException("* operation on different shapes")

  lazy val normalizeShape: Fuzz[T] = shape match {
    case Gaussian => this
    case Box => RelativeFuzz(Box.toGaussianRelative(tolerance), Gaussian)
  }

  def toString(t: T): String = absolute(t).map(_.toString(t)).getOrElse("")

  /**
    * Yields the actual probability density for the value t.
    *
    * @param t a particular T value, for which we want the probability density.
    * @return the probability density.
    */
  def probabilityDensity(t: T): Double = shape.probabilityDensity(tv.toDouble(t), tolerance)

  /**
    * Determine the range +- t within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @return the value of t at which the probability density is exactly transitions from likely to not likely.
    */
  def likely: T = tv.fromDouble(shape.likely(tolerance))
}

case class AbsoluteFuzz[T: Valuable](magnitude: T, shape: Shape) extends Fuzz[T] {
  private val tv = implicitly[Valuable[T]]

  /**
    * This method takes a value of T on which to base a relative fuzz value.
    * NOTE: if t is zero, we will return None, which corresponds to an Exact number.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def relative(t: T): Option[RelativeFuzz[T]] = {
    Try(RelativeFuzz(tv.toDouble(tv.normalize(tv.div(magnitude, t))), shape)).toOption
  }

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return an (optional) transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzz[T] = AbsoluteFuzz(func(magnitude), shape)

  /**
    * Return either a RelativeFuzz equivalent or this, according to relativeStyle.
    *
    * @param t             the magnitude of the relevant Number.
    * @param relativeStyle if true then convert to Absolute otherwise wrap this in Some().
    * @return the (optional) Fuzz as a Relative or Absolute Fuzz, according to relative.
    */
  def normalize(t: T, relativeStyle: Boolean): Option[Fuzz[T]] = if (relativeStyle) relative(t) else Some(this)

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
      case AbsoluteFuzz(m, _) =>
        AbsoluteFuzz(tv.fromDouble(Gaussian.convolutionSum(tv.toDouble(magnitude), tv.toDouble(m))), shape)
      case _ =>
        throw FuzzyNumberException("* operation on different styles")
    }
  else
    throw FuzzyNumberException("* operation on different shapes")

  lazy val normalizeShape: Fuzz[T] = shape match {
    case Gaussian => this
    case Box => AbsoluteFuzz(Box.toGaussianAbsolute(magnitude), Gaussian)
  }

  private def round(d: Double, i: Int) = math.round(d * math.pow(10, i)) * math.pow(10, -i)

  private val numberR = """-?\d+\.\d+E([\-+]?\d+)""".r
  private val noExponent = "+00"

  /**
    * Method to render this Fuzz according to the nominal value t.
    *
    * CONSIDER cleaning this method up a bit.
    *
    * @param t a T value.
    *  @return a String which is the textual rendering of t with this Fuzz applied.
    */
  def toString(t: T): String = {
    val eString = tv.render(t) match {
      case numberR(e) => e
      case _ => noExponent
    }
    val exponent = Integer.parseInt(eString)
    val scientificSuffix = eString match {
      case `noExponent` => ""
      case x => s"E$x"
    }
    val scaledM = tv.toDouble(magnitude) * math.pow(10, -exponent)
    val roundedM = round(scaledM, 2 - math.log10(scaledM).toInt)
    //      if (scaledM > 0.01) // TODO let's do this unusual adjustment later
    val scaledT = tv.scale(t, math.pow(10, -exponent))
    val q = f"$roundedM%.99f".substring(2) // drop the "0."
    val (qPrefix, qSuffix) = q.toCharArray.span(_ == '0')
    val (qPreSuffix, _) = qSuffix.span(_ != '0')
    val adjust = qPreSuffix.length - 2
    val mScaledAndRounded = round(scaledM, qPrefix.length + 2 + adjust) * math.pow(10, qPrefix.length)
    val yq = mScaledAndRounded.toString.substring(2).padTo(2 + adjust, '0').substring(0, 2 + adjust)
    val brackets = if (shape == Gaussian) "()" else "[]"
    // CONSIDER changing the padding "0" value to be "5".
    val mask = new String(qPrefix) + "0" * (2 + adjust) + brackets.head + yq + brackets.tail.head
    val (zPrefix, zSuffix) = tv.render(scaledT).toCharArray.span(_ != '.')
    new String(zPrefix) + "." + Fuzz.zipStrings(new String(zSuffix).substring(1), mask) + scientificSuffix
  }

  /**
    * Yields the actual probability density for the value t.
    *
    * @param t a particular T value, for which we want the probability density.
    * @return the probability density.
    */
  def probabilityDensity(t: T): Double = shape.probabilityDensity(tv.toDouble(t), tv.toDouble(magnitude))

  /**
    * Determine the range +- x within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @return the value of x at which the probability density is exactly transitions from likely to not likely.
    */
  def likely: T = tv.fromDouble(shape.likely(tv.toDouble(magnitude)))
}

object Fuzz {


  /**
    * Combine the fuzz values using a convolution.
    * The order of fuzz1 and fuzz2 is not significant.
    * Note that we normalize the style of each fuzz according to the value of relative.
    * Note also that we normalize the shape of each fuzz to ensure Gaussian, since we cannot combine Box shapes into Box
    * (we could combine Box shapes into trapezoids but who needs that?).
    *
    * @param t        the magnitude of the resulting Number.
    * @param relative true if we are multiplying, false if we are adding.
    * @param fuzz1    one of the (optional) Fuzz values.
    * @param fuzz2    the other of the (optional) Fuzz values.
    * @tparam T the underlying type of the Fuzz.
    * @return an Option of Fuzz[T].
    */
  def combine[T: Valuable](t: T, relative: Boolean, fuzz1: Option[Fuzz[T]], fuzz2: Option[Fuzz[T]]): Option[Fuzz[T]] =
    (normalizeFuzz(fuzz1, t, relative), normalizeFuzz(fuzz2, t, relative)) match {
      case (Some(f1), Some(f2)) => Some(f1.normalizeShape * f2.normalizeShape)
      case (Some(f1), None) => Some(f1)
      case (None, Some(f2)) => Some(f2)
      case _ => None
    }

  /**
    * Map the fuzz value with a function (typically the derivative of the function being applied to the Fuzzy quantity).
    * Note that we normalize the style of each fuzz according to the value of relative.
    * Note also that we normalize the shape of each fuzz to ensure Gaussian, since we cannot combine Box shapes into Box
    * (we could combine Box shapes into trapezoids but who needs that?).
    *
    * @param t        the magnitude of the resulting Number.
    * @param relative true if we are multiplying, false if we are adding.
    * @param g        the function with which to transform the given Fuzz value
    * @param fuzz     one of the (optional) Fuzz values.
    * @tparam T the underlying type of the Fuzz.
    * @return an Option of Fuzz[T].
    */
  def map[T: Valuable](t: T, relative: Boolean, g: T => T, fuzz: Option[Fuzz[T]]): Option[Fuzz[T]] =
    normalizeFuzz(fuzz, t, relative) match {
      case Some(f1) => Some(f1.transform(g))
      case _ => None
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

  def zipStrings(v: String, t: String): String = {
    val cCs = ((LazyList.from(v.toCharArray.toList) :++ LazyList.continually('0')) zip t.toCharArray.toList).toList
    val r: List[Char] = cCs map {
      case (a, b) => if (b == '0') a else b
    }
    new String(r.toArray)
  }

  private def normalizeFuzz[T: Valuable](t: T, relative: Boolean, f: Fuzz[T]) =
    f match {
      case a@AbsoluteFuzz(_, _) => if (relative) a.relative(t) else Some(f)
      case r@RelativeFuzz(_, _) => if (relative) Some(f) else r.absolute(t)
    }
}

/**
  * Describes a probability density function for a continuous distribution.
  * NOTE: this isn't suitable for discrete distributions, obviously.
  */
trait Shape {
  /**
    * Determine the approximate probability density for this shape at the point x where l signifies the extent of the PDF.
    *
    * @param x the x value.
    * @param l the extent of the PDF (for example, the standard deviation, for a Gaussian).
    * @return the value of the probability density at x.
    */
  def probabilityDensity(x: Double, l: Double): Double

  /**
    * Determine the range +- x within which a deviation is considered within tolerance.
    *
    * @param l the extent of the PDF (for example, the standard deviation, for a Gaussian).
    * @return the value of x at which the probability density is exactly transitions from likely to not likely.
    */
  def likely(l: Double): Double
}

case object Box extends Shape {
  /**
    * See, for example, https://www.unf.edu/~cwinton/html/cop4300/s09/class.notes/Distributions1.pdf
    */
  private val uniformToGaussian = 1.0 / math.sqrt(3)

  /**
    * This method is to simulate a uniform distribution
    * with a normal distribution.
    *
    * @param x half the length of the basis of the uniform distribution.
    * @return x/2 which will be used as the standard deviation.
    */
  def toGaussianRelative(x: Double): Double = x * uniformToGaussian

  /**
    * This method is to simulate a uniform distribution
    * with a normal distribution.
    *
    * @param t the magnitude of the Box distribution.
    * @return t/2 which will be used as the standard deviation.
    */
  def toGaussianAbsolute[T: Valuable](t: T): T = implicitly[Valuable[T]].scale(t, uniformToGaussian)

  /**
    * Determine the probability density for this shape at the point x where l signifies the extent of the PDF.
    *
    * @param x the x value (ignored)
    * @param l the half-width of a Box.
    */
  def probabilityDensity(x: Double, l: Double): Double = 0.5 / l

  /**
    * Determine the range +- x within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @param l the half-width of a Box.
    * @return the value of x at which the probability density transitions from possible to impossible.
    */
  def likely(l: Double): Double = l /2
}

case object Gaussian extends Shape {
  /**
    * Get the convolution of sum of two Gaussian distributions.
    *
    * This follows the fact that Var(X + Y) = Var(X) + Var(Y)
    *
    * See https://en.wikipedia.org/wiki/Variance
    *
    * @param sigma1 the first standard deviation.
    * @param sigma2 the second standard deviation.
    * @return a double which is the root mean square of the sigmas.
    */
  def convolutionSum(sigma1: Double, sigma2: Double): Double = math.sqrt(sigma1 * sigma1 + sigma2 * sigma2)

  /**
    * For the convolution of the product of two (independent) Gaussian distributions (see https://en.wikipedia.org/wiki/Variance):
    *
    * Var(X Y) = mux mux Var(Y) + muy muy Var(X) + Var(X) Var(Y)
    *
    * Therefore, (sigma(x*y))**2 = (mux sigma(y))**2 + (muy sigma(x))**2 + (sigma(x) sigma(y))**2
    *
    * Or, dividing by (mux muy)**2 (i.e. using relative variations)
    *
    * sigma(x/mux 8 y/muy)**2 = sigma(x/mux)**2 + sigma(y/muy)**2 + (sigma(x/mux) sigma(y/muy))**2
    */
  def convolutionProduct(sigma1: Double, sigma2: Double): Double = math.sqrt(sigma1 * sigma1 + sigma2 * sigma2 + sigma1 * sigma2)

  /**
    * Determine the APPROXIMATE probability density for this shape at the point x where l signifies the extent of the PDF.
    * This is not intended for precision.
    *
    * @param x     the x value.
    * @param sigma the standard deviation of a Box.
    */
  def probabilityDensity(x: Double, sigma: Double): Double = {
    val y = math.abs(x)
    if (y < 1E-1) 1
    else if (y < 0.47) 0.51
    else if (y < 1) 0.32
    else if (y < 2) 0.05
    else if (y < 3) 0.0025
    else 0
  }

  /**
    * Determine the range +- x within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @param l the standard deviation.
    * @return the value of x at which the probability density is exactly 0.5.
    */
  def likely(l: Double): Double = l * 0.475
}

trait Fuzzy[T] {
  val fuzz: Option[Fuzz[T]]
}

trait Valuable[T] extends Fractional[T] {
  def render(t: T): String

  def fromDouble(x: Double): T

  def scale(x: T, f: Double): T

  /**
    * Method to yield a "normalized" version of x.
    * For a Numeric object, this implies the absolute value, i.e. with no sign.
    *
    * @param x the value.
    * @return the value, without any sign.
    */
  def normalize(x: T): T
}

trait ValuableDouble extends Valuable[Double] with DoubleIsFractional with Ordering.Double.IeeeOrdering {
  def render(t: Double): String = {
    lazy val asScientific: String = f"$t%.20E"
    val z = f"${t.toDouble}%.99f"
    val (prefix, suffix) = z.toCharArray.span(x => x != '.')
    val sevenZeroes = "0000000".toCharArray
    if (prefix.endsWith(sevenZeroes)) asScientific
    else if (suffix.tail.startsWith(sevenZeroes)) asScientific
    else z
  }

  def fromDouble(x: Double): Double = x

  def scale(x: Double, f: Double): Double = x * f

  /**
    * Method to yield a "normalized" version of x.
    * For a Numeric object, this implies the absolute value, i.e. with no sign.
    *
    * @param x the value.
    * @return the value, without any sign.
    */
  def normalize(x: Double): Double = math.abs(x)
}

object Valuable {

  implicit object ValuableDouble extends ValuableDouble

}