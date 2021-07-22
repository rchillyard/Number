package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Fuzziness.toDecimalPower

import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering
import scala.util.Try

/**
  * Type constructor which will give fuzzy behavior to a type T.
  *
  * @tparam T the underlying type of the fuzziness. Usually Double for fuzzy numerics.
  */
trait Fuzziness[T] {
  /**
    * One of two shapes for the probability density function:
    * Gaussian (normal distribution);
    * Box (uniform distribution).
    */
  val shape: Shape

  /**
    * True if this is relative Fuzz as opposed to absolute fuzz.
    */
  val style: Boolean

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return an (optional) transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzziness[T]

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * There are two different Fuzz[T] shapes, and they clearly have different effects.
    * When the shapes are absolute, this operation is suitable for addition of Numbers.
    * When the shapes are relative, this operation is suitable for multiplication of Numbers.
    *
    * Whether or not this is a true convolution, I'm not sure.
    * But is an operation to combine two probability density functions and, as such, f * g = g * f.
    *
    * @param convolute   the convolute, which must have the same shape as this.
    * @param independent true if the fuzz distributions are independent.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T]

  /**
    * Method to possibly change the style of this Fuzz[T}.
    *
    * @param t        the magnitude of the relevant Number.
    * @param relative if true then change to Relative (if Absolute).
    * @return the (optional) Fuzz as a Relative or Absolute Fuzz, according to relative.
    */
  def normalize(t: T, relative: Boolean): Option[Fuzziness[T]]

  /**
    * Method to convert this Fuzz[T] into a Fuzz[T] with Gaussian shape.
    *
    * @return the equivalent Fuzz[T] with Gaussian shape.
    */
  def normalizeShape: Fuzziness[T]

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
  def likely(p: Double): T
}

/**
  * Relative fuzziness.
  *
  * @param tolerance the error bound.
  * @param shape     the shape (Uniform, Gaussian, etc.)
  * @tparam T the underlying type of the fuzziness. Usually Double for fuzzy numerics [must have Valuable].
  */
case class RelativeFuzz[T: Valuable](tolerance: Double, shape: Shape) extends Fuzziness[T] {

  private val tv: Valuable[T] = implicitly[Valuable[T]]

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return a transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzziness[T] = RelativeFuzz(tv.toDouble(func(tv.fromDouble(tolerance))), shape)

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
  def normalize(t: T, relative: Boolean): Option[Fuzziness[T]] = if (relative) Some(this) else absolute(t)

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * This operation is suitable for multiplication of Numbers.
    *
    * @param convolute   the convolute, which must have the same shape as this.
    * @param independent true if the distributions are independent.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] = if (this.shape == convolute.shape)
    convolute match {
      case RelativeFuzz(t, Gaussian) => RelativeFuzz(Gaussian.convolutionProduct(tolerance, t, independent), shape)
      case _ => throw FuzzyNumberException("* operation on different styles")
    } else throw FuzzyNumberException("* operation on different shapes")

  /**
    * Yield a Fuzz[T] that is Gaussian (either this or derivative of this).
    */
  lazy val normalizeShape: Fuzziness[T] = shape match {
    case Gaussian => this
    case Box => RelativeFuzz(Box.toGaussianRelative(tolerance), Gaussian)
  }

  /**
    * Render this Fuzz as with a given T value.
    *
    * @param t the T value.
    * @return a String which is the textual rendering of t with this Fuzz applied.
    */
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
    * @param p the confidence we wish to have in the likelihood: typical value: 0.5
    * @return the value of t at which the probability density is exactly transitions from likely to not likely.
    */
  def likely(p: Double): T = tv.fromDouble(shape.likely(tolerance, p))

  /**
    * True.
    */
  val style: Boolean = true
}

/**
  * Absolute Fuzziness.
  *
  * @param magnitude the magnitude of the fuzz.
  * @param shape     the shape of the fuzz.
  * @tparam T the underlying type of the fuzziness. Usually Double for fuzzy numerics.
  */
case class AbsoluteFuzz[T: Valuable](magnitude: T, shape: Shape) extends Fuzziness[T] {
  private val tv = implicitly[Valuable[T]]

  /**
    * This method takes a value of T on which to base a relative fuzz value.
    * NOTE: if t is zero, we will return None, which corresponds to an Exact number.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def relative(t: T): Option[RelativeFuzz[T]] =
    Try(RelativeFuzz(tv.toDouble(tv.normalize(tv.div(magnitude, t))), shape)).toOption

  /**
    * Transform this Fuzz[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzz[T].
    * @return an (optional) transformed version of Fuzz[T].
    */
  def transform(func: T => T): Fuzziness[T] = AbsoluteFuzz(func(magnitude), shape)

  /**
    * Return either a RelativeFuzz equivalent or this, according to relativeStyle.
    *
    * @param t             the magnitude of the relevant Number.
    * @param relativeStyle if true then convert to Absolute otherwise wrap this in Some().
    * @return the (optional) Fuzz as a Relative or Absolute Fuzz, according to relative.
    */
  def normalize(t: T, relativeStyle: Boolean): Option[Fuzziness[T]] = if (relativeStyle) relative(t) else Some(this)

  /**
    * Perform a convolution on this Fuzz[T] with the given addend.
    * There are two different Fuzz[T] shapes, and they clearly have different effects.
    * This operation is suitable for addition of Numbers.
    *
    * @param convolute   the convolute, which must have the same shape as this.
    * @param independent ignored.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] =
    if (this.shape == convolute.shape)
      convolute match {
        case AbsoluteFuzz(m, _) =>
          AbsoluteFuzz(tv.fromDouble(Gaussian.convolutionSum(tv.toDouble(magnitude), tv.toDouble(m))), shape)
        case _ =>
          throw FuzzyNumberException("* operation on different styles")
      }
    else
      throw FuzzyNumberException("* operation on different shapes")

  lazy val normalizeShape: Fuzziness[T] = shape match {
    case Gaussian => this
    case Box => AbsoluteFuzz(Box.toGaussianAbsolute(magnitude), Gaussian)
  }

  /**
    * Method to do accurate rounding of Double.
    *
    * @param x the double value to be rounded.
    * @param n the number of places to be rounded to.
    * @return the rounded value of x.
    */
  def round(x: Double, n: Int): Double = BigDecimal(BigDecimal(math.round(toDecimalPower(x, n)).toInt).bigDecimal.movePointLeft(n)).toDouble

  /**
    * Method to render this Fuzz according to the nominal value t.
    *
    * CONSIDER cleaning this method up a bit.
    *
    * @param t a T value.
    * @return a String which is the textual rendering of t with this Fuzz applied.
    */
  def toString(t: T): String = {
    val eString = tv.render(t) match {
      case AbsoluteFuzz.numberR(e) => e
      case _ => noExponent
    }
    val exponent = Integer.parseInt(eString)
    val scientificSuffix = eString match {
      case `noExponent` => ""
      case x => s"E$x"
    }
    val scaledM = toDecimalPower(tv.toDouble(magnitude), -exponent)
    val d = math.log10(scaledM).toInt
    val roundedM = round(scaledM, 2 - d)
    //      if (scaledM > 0.01) // TODO let's do this unusual adjustment later
    val scaledT = tv.scale(t, toDecimalPower(1, -exponent))
    val q = f"$roundedM%.99f".substring(2) // XXX drop the "0."
    val (qPrefix, qSuffix) = q.toCharArray.span(_ == '0')
    val (qPreSuffix, _) = qSuffix.span(_ != '0')
    val adjust = qPreSuffix.length - 2
    val mScaledAndRounded = toDecimalPower(round(scaledM, qPrefix.length + 2 + adjust), qPrefix.length)
    val yq = mScaledAndRounded.toString.substring(2).padTo(2 + adjust, '0').substring(0, 2 + adjust)
    val brackets = if (shape == Gaussian) "()" else "[]"
    // CONSIDER changing the padding "0" value to be "5".
    val mask = new String(qPrefix) + "0" * (2 + adjust) + brackets.head + yq + brackets.tail.head
    val (zPrefix, zSuffix) = tv.render(scaledT).toCharArray.span(_ != '.')
    new String(zPrefix) + "." + Fuzziness.zipStrings(new String(zSuffix).substring(1), mask) + scientificSuffix
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
  def likely(p: Double): T = tv.fromDouble(shape.likely(tv.toDouble(magnitude), 0.5))

  /**
    * False.
    */
  val style: Boolean = false

  private val noExponent = "+00"
}

object AbsoluteFuzz {
  private val numberR = """-?\d+\.\d+E([\-+]?\d+)""".r
}

object Fuzziness {

  /**
    * Method to return a transform function which multiplies a T value by another T value.
    *
    * @param k a constant T value (the scale factor).
    * @tparam T the underlying type of the input and output.
    * @return a function which can be used by transform.
    */
  def scale[T: Valuable](k: T): T => T = implicitly[Valuable[T]].times(_, k)

  /**
    * Method to yield a transformation (i.e. a Fuzz[T] => Fuzz[T]) based on a scale constant k.
    *
    * @param k the scale constant.
    * @tparam T the underlying type.
    * @return a function which will transform a Fuzzy[T] into a Fuzzy[T].
    */
  def scaleTransform[T: Valuable](k: T): Fuzziness[T] => Fuzziness[T] = _.transform(scale(k))

  /**
    * Scale the fuzz values by the two given coefficients.
    *
    * @param fuzz         the fuzz values (a Tuple).
    * @param coefficients the coefficients (a Tuple).
    * @return the scaled fuzz values (a Tuple).
    */
  def applyCoefficients[T: Valuable](fuzz: (Option[Fuzziness[T]], Option[Fuzziness[T]]), coefficients: Option[(T, T)]): (Option[Fuzziness[T]], Option[Fuzziness[T]]) =
    coefficients match {
      case Some((a, b)) =>
        val f1o = fuzz._1 map scaleTransform(a)
        val f2o = fuzz._2 map scaleTransform(b)
        (f1o, f2o)
      case _ => fuzz
    }

  /**
    * Combine the fuzz values using a convolution.
    * The order of fuzz1 and fuzz2 is not significant.
    * Note that we normalize the style of each fuzz according to the value of relative.
    * Note also that we normalize the shape of each fuzz to ensure Gaussian, since we cannot combine Box shapes into Box
    * (we could combine Box shapes into trapezoids but who needs that?).
    *
    * @param t1       the magnitude of the first operand.
    * @param t2       the magnitude of the second operand.
    * @param relative true if we are multiplying, false if we are adding.
    * @param fuzz     a Tuple of the two optional Fuzz values.
    * @tparam T the underlying type of the Fuzz.
    * @return an Option of Fuzz[T].
    */
  def combine[T: Valuable](t1: T, t2: T, relative: Boolean, independent: Boolean)(fuzz: (Option[Fuzziness[T]], Option[Fuzziness[T]])): Option[Fuzziness[T]] = {
    val f1o = doNormalize(fuzz._1, t1, relative)
    val f2o = doNormalize(fuzz._2, t2, relative)
    (f1o, f2o) match {
      case (Some(f1), Some(f2)) => Some(f1.normalizeShape.*(f2.normalizeShape, independent))
      case (Some(f1), None) => Some(f1)
      case (None, Some(f2)) => Some(f2)
      case _ => None
    }
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
  def map[T: Valuable](t: T, relative: Boolean, g: T => T, fuzz: Option[Fuzziness[T]]): Option[Fuzziness[T]] =
    doNormalize(fuzz, t, relative) match {
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
  def doNormalize[T: Valuable](fuzz: Option[Fuzziness[T]], t: T, relative: Boolean): Option[Fuzziness[T]] =
    fuzz.flatMap(f => doNormalize(t, relative, f))

  def zipStrings(v: String, t: String): String = {
    val cCs = ((LazyList.from(v.toCharArray.toList) :++ LazyList.continually('0')) zip t.toCharArray.toList).toList
    val r: List[Char] = cCs map {
      case (a, b) => if (b == '0') a else b
    }
    new String(r.toArray)
  }

  /**
    * Apply a decimal exponent of n to x and return the new value.
    * CONSIDER using BigDecimal for more precision (see AbsoluteFuzz.round)
    *
    * @param x a Double.
    * @param n the size of the exponent.
    * @return the result.
    */
  def toDecimalPower(x: Double, n: Int): Double = x * math.pow(10, n)

  private def doNormalize[T](t: T, relative: Boolean, f: Fuzziness[T]) =
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
    * @param p the confidence that we wish to place on the likelihood: typical value is 0.5.
    * @return the value of x at which the probability density is exactly transitions from likely to not likely.
    */
  def likely(l: Double, p: Double): Double
}

/**
  * Uniform probability density over a specific range, otherwise zero.
  */
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
    * @param p ignored
    * @return the value of x at which the probability density transitions from possible to impossible.
    */
  def likely(l: Double, p: Double): Double = l / 2
}

/**
  * A "normal" probability distribution function.
  */
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
    * For the convolution of the product of two (independent) Gaussian distributions (see https://en.wikipedia.org/wiki/Variance).
    * You must not use this expression when multiplying a fuzzy number by itself, for example, because then they are not independent.
    *
    * Var(X Y) = mux mux Var(Y) + muy muy Var(X) + Var(X) Var(Y)
    *
    * Therefore, (sigma(x*y))**2 = (mux sigma(y))**2 + (muy sigma(x))**2 + (sigma(x) sigma(y))**2
    *
    * Or, dividing by (mux muy)**2 (i.e. using relative variations)
    *
    * sigma(x/mux 8 y/muy)**2 = sigma(x/mux)**2 + sigma(y/muy)**2 + (sigma(x/mux) sigma(y/muy))**2
    *
    * @param sigma1      the first standard deviation.
    * @param sigma2      the second standard deviation.
    * @param independent whether or not the distributions are independent.
    * @return
    */
  def convolutionProduct(sigma1: Double, sigma2: Double, independent: Boolean): Double =
    if (independent) math.sqrt(sigma1 * sigma1 + sigma2 * sigma2 + sigma1 * sigma2)
    else sigma1 + sigma2

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
    * This is based on the table of erfc(p) value.
    *
    * @param l the standard deviation.
    * @param p the confidence desired for the likelihood.
    *          NOTE: only certain values are supported: 0.05, 0.1, 0.25 0.5, 0.75, 0.95.
    * @return the value of x at which the cumulative probability is p.
    */
  def likely(l: Double, p: Double): Double = l * p match {
    case 0.05 => 1.38
    case 0.1 => 1.16
    case 0.25 => 0.815
    case 0.5 => 0.475
    case 0.75 => 0.225
    case 0.95 => 0.045
    case _ => 0.475
  }
}

trait Fuzz[T] {
  val fuzz: Option[Fuzziness[T]]
}

/**
  * Type class Valuable[T].
  *
  * @tparam T the underlying type of this Valuable.
  */
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
    val z = f"$t%.99f"
    val (prefix, suffix) = z.toCharArray.span(x => x != '.')
    val sevenZeroes = "0000000".toCharArray
    if (prefix.endsWith(sevenZeroes)) asScientific
    else if (suffix.tail.startsWith(sevenZeroes)) asScientific
    else z
  }

  def fromDouble(x: Double): Double = x

  /**
    * Scale the parameter x by the constant factor f.
    *
    * @param x a value.
    * @param f a factor.
    * @return x * f.
    */
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