package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Fuzziness.toDecimalPower
import com.phasmidsoftware.number.core.Valuable.ValuableDouble
import org.apache.commons.math3.special.Erf.erfInv
import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering
import scala.util.Try

/**
  * This trait models the behavior of fuzziness.
  *
  * See also related trait Fuzzy[X].
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
    * True if this is relative Fuzziness as opposed to absolute fuzz.
    */
  val style: Boolean

  /**
    * Transform this Fuzziness[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzziness[T].
    * @return an (optional) transformed version of Fuzziness[T].
    */
  def transform[U: Valuable, V: Valuable](func: T => V)(t: T): Fuzziness[U]

  /**
    * Perform a convolution on this Fuzziness[T] with the given addend.
    * There are two different Fuzziness[T] shapes, and they clearly have different effects.
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
    * Method to possibly change the style of this Fuzziness[T}.
    *
    * @param t        the magnitude of the relevant Number.
    * @param relative if true then change to Relative (if Absolute).
    * @return the (optional) Fuzziness as a Relative or Absolute Fuzziness, according to relative.
    */
  def normalize(t: T, relative: Boolean): Option[Fuzziness[T]]

  /**
    * Method to convert this Fuzziness[T] into a Fuzziness[T] with Gaussian shape.
    *
    * @return the equivalent Fuzziness[T] with Gaussian shape.
    */
  def normalizeShape: Fuzziness[T]

  /**
    * Method to yield a String to render the given T value.
    *
    * @param t a T value.
    * @return a String which is the textual rendering of t with this Fuzziness applied.
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
    * In other words get the wiggle room.
    *
    * NOTE effectively, this method converts a Gaussian distribution into a Box distribution.
    * CONSIDER refactoring to take advantage of that equivalence.
    *
    * @param p the confidence we wish to have in the result: typical value: 0.5
    * @return the value of t at which the probability density is exactly transitions from likely to not likely.
    */
  def wiggle(p: Double): T
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
    * Transform this Fuzziness[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzziness[T].
    * @param t    the value of t (i.e. the input value) which will be passed into func.
    * @tparam U the underlying type of the result.
    * @tparam V the type of the quotient of T/U.
    * @return a transformed version of Fuzziness[U].
    */
  def transform[U: Valuable, V: Valuable](func: T => V)(t: T): Fuzziness[U] = {
    val duByDt: V = func(t)
    val dU: U = implicitly[Valuable[U]].multiply(implicitly[Valuable[T]].fromDouble(tolerance), duByDt)
    RelativeFuzz[U](implicitly[Valuable[U]].toDouble(dU), shape)
  }

  /**
    * This method takes a value of T on which to base a relative fuzz value.
    *
    * @param t the nominal value of the fuzzy number.
    * @return an optional RelativeFuzz[T]
    */
  def absolute(t: T): Option[AbsoluteFuzz[T]] = {
    // CONSIDER using multiply, scale, etc.
    Try(AbsoluteFuzz(tv.normalize(tv.times(tv.fromDouble(tolerance), t)), shape)).toOption
  }

  /**
    * Return either a AbsoluteFuzz equivalent or this, according to relative.
    *
    * @param t        the magnitude of the relevant Number.
    * @param relative if true then convert to RelativeFuzz otherwise wrap this in Some().
    * @return the (optional) Fuzziness as a Relative or Absolute Fuzziness, according to relative.
    */
  def normalize(t: T, relative: Boolean): Option[Fuzziness[T]] = if (relative) Some(this) else absolute(t)

  /**
    * Perform a convolution on this Fuzziness[T] with the given addend.
    * This operation is suitable for multiplication of Numbers.
    *
    * @param convolute   the convolute, which must have the same shape as this.
    * @param independent true if the distributions are independent.
    * @return the convolution of this and the convolute.
    */
  def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] = if (this.shape == convolute.shape)
    convolute match {
      case RelativeFuzz(t, Box) => RelativeFuzz(tolerance + t, shape)
      case RelativeFuzz(t, _) => RelativeFuzz(Gaussian.convolutionProduct(tolerance, t, independent), shape)
      case _ => throw FuzzyNumberException("* operation on different styles")
    }
  else
    throw FuzzyNumberException("* operation on different shapes")

  /**
    * Yield a Fuzziness[T] that is Gaussian (either this or derivative of this).
    */
  lazy val normalizeShape: Fuzziness[T] = shape match {
    case Gaussian => this
    case Box => RelativeFuzz(Box.toGaussianRelative(tolerance), Gaussian)
  }

  /**
    * Render this Fuzziness as with a given T value.
    *
    * @param t the T value.
    * @return a String which is the textual rendering of t with this Fuzziness applied.
    */
  def toString(t: T): String = absolute(t).map(_.toString(t)).getOrElse("")

  /**
    * Yields the actual probability density for the value t.
    *
    * TEST me (not used)
    *
    * @param t a particular T value, for which we want the probability density.
    * @return the probability density.
    */
  def probabilityDensity(t: T): Double = shape.probabilityDensity(tv.toDouble(t), tolerance)

  /**
    * Determine the range +- t within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    * In other words get the wiggle room.
    *
    * @param p the confidence we wish to have in the result: typical value: 0.5
    * @return the value of t at which the probability density is exactly transitions from likely to not likely.
    */
  def wiggle(p: Double): T = tv.fromDouble(shape.wiggle(tolerance, p))

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
    Try(RelativeFuzz(tv.toDouble(tv.normalize(tv.div(magnitude, t))), shape)(tv)).toOption

  /**
    * Transform this Fuzziness[T] according to func.
    * Typically, func will be the derivative of the relevant Number function.
    *
    * @param func the function to apply to this Fuzziness[T].
    * @return an (optional) transformed version of Fuzziness[T].
    */
  def transform[U: Valuable, V: Valuable](func: T => V)(t: T): Fuzziness[U] = {
    val duByDt: V = func(t)
    val dU: U = implicitly[Valuable[U]].multiply(magnitude, duByDt)
    AbsoluteFuzz(dU, shape)
  }

  /**
    * Return either a RelativeFuzz equivalent or this, according to relativeStyle.
    *
    * @param t             the magnitude of the relevant Number.
    * @param relativeStyle if true then convert to Absolute otherwise wrap this in Some().
    * @return the (optional) Fuzziness as a Relative or Absolute Fuzziness, according to relative.
    */
  def normalize(t: T, relativeStyle: Boolean): Option[Fuzziness[T]] = if (relativeStyle) relative(t) else Some(this)

  /**
    * Perform a convolution on this Fuzziness[T] with the given addend.
    * There are two different Fuzziness[T] shapes, and they clearly have different effects.
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
    * Method to render this Fuzziness according to the nominal value t.
    *
    * CONSIDER cleaning this method up a bit.
    *
    * @param t a T value.
    * @return a String which is the textual rendering of t with this Fuzziness applied.
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
    * In other words get the wiggle room.
    *
    * @param p the confidence we wish to have in the result: typical value: 0.5
    * @return the value of x at which the probability density is exactly transitions from likely to not likely.
    */
  def wiggle(p: Double): T = tv.fromDouble(shape.wiggle(tv.toDouble(magnitude), p))

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
    * Method to yield a transformation (i.e. a Fuzziness[T] => Fuzziness[T]) based on a scale constant k.
    *
    * @param k the scale constant.
    * @tparam T the underlying type.
    * @return a function which will transform a Fuzziness[T] into a Fuzziness[T].
    */
  def scaleTransform[T: Valuable](k: Double): Fuzziness[T] => Fuzziness[T] =
    tf => tf.transform(_ => implicitly[Valuable[T]].fromDouble(k))(implicitly[Valuable[T]].fromInt(1))


  /**
    * Scale the fuzz values by the two given coefficients.
    *
    * @param fuzz         the fuzz values (a Tuple), deltaX/X and deltaY/Y.
    * @param coefficients the coefficients (a Tuple): the coefficients of deltaX/X and deltaY/Y respectively.
    * @return the scaled fuzz values (a Tuple).
    */
  def applyCoefficients[T: Valuable](fuzz: (Option[Fuzziness[T]], Option[Fuzziness[T]]), coefficients: Option[(T, T)]): (Option[Fuzziness[T]], Option[Fuzziness[T]]) =
    coefficients match {
      case Some((a, b)) =>
        val f1o = fuzz._1 map scaleTransform(implicitly[Valuable[T]].toDouble(a))
        val f2o = fuzz._2 map scaleTransform(implicitly[Valuable[T]].toDouble(b))
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
    * @param fuzz     a Tuple of the two optional Fuzziness values.
    * @tparam T the underlying type of the Fuzziness.
    * @return an Option of Fuzziness[T].
    */
  def combine[T: Valuable](t1: T, t2: T, relative: Boolean, independent: Boolean)(fuzz: (Option[Fuzziness[T]], Option[Fuzziness[T]])): Option[Fuzziness[T]] = {
    val f1o = doNormalize(fuzz._1, t1, relative)
    val f2o = doNormalize(fuzz._2, t2, relative)
    (f1o, f2o) match {
      case (Some(f1), Some(f2)) if relative && f1.shape == Box && f2.shape == Box => Some(f1.*(f2, independent))
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
    * @param t        the magnitude of the input Number.
    * @param relative true if we are multiplying, false if we are adding.
    * @param g        the function with which to transform the given Fuzziness value
    * @param fuzz     one of the (optional) Fuzziness values.
    * @tparam T the underlying type of the incoming Fuzziness.
    * @tparam U the underlying type of the resulting Fuzziness.
    * @tparam V the underlying type of the result divided by the input.
    * @return an Option of Fuzziness[T].
    */
  def map[T: Valuable, U: Valuable, V: Valuable](t: T, u: U, relative: Boolean, g: T => V, fuzz: Option[Fuzziness[T]]): Option[Fuzziness[U]] = {
    val q: Option[Fuzziness[U]] = fuzz match {
      case Some(f1) => Some(f1.transform(g)(t))
      case _ => None
    }
    doNormalize(q, u, relative)
  }

  /**
    * This method creates fuzz based on the practical limitations of representations and functions in double-precision arithmetic.
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return the approximate precision for a floating point operation, expressed in terms of RelativeFuzz.
    */
  def createFuzz(relativePrecision: Int): RelativeFuzz[Double] =
    RelativeFuzz[Double](DoublePrecisionTolerance * (1 << relativePrecision), Box)(ValuableDouble)

  /**
    * Normalize the magnitude qualifier of the given fuzz according to relative.
    * If relative is true then the returned value will be a RelativeFuzz.
    * If relative is false then the returned value will be an AbsoluteFuzz.
    *
    * @param fuzz     the optional Fuzziness to work on.
    * @param t        the value of T that the fuzz IS or result SHOULD BE relative to.
    * @param relative if true then return optional relative fuzz, else absolute fuzz.
    * @tparam T the underlying type of the Fuzziness.
    * @return the optional Fuzziness value which is equivalent (or identical) to fuzz, according to the value of relative.
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
  def wiggle(l: Double, p: Double): Double
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
    * @return the value of the probability density at x.
    */
  def probabilityDensity(x: Double, l: Double): Double = 0.5 / l

  /**
    * Determine the range +- x within which a deviation is considered within tolerance and where
    * l signifies the extent of the PDF.
    *
    * @param l the half-width of a Box.
    * @param p the confidence that we wish to place on the likelihood: typical value is 0.5.
    *          Unless is is either 0 or 1, the actual p value is ignored.
    * @return the value of x at which the probability density transitions from possible to impossible.
    */
  def wiggle(l: Double, p: Double): Double = p match {
    case 0.0 => Double.PositiveInfinity
    case 1.0 => 0
    case _ => l / 2
  }

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
    * TODO use erf instead.
    *
    * @param x     the x value.
    * @param sigma the standard deviation of a Box.
    * @return the value of the probability density at x.
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
    * Determine the "wiggle room" for a particular probability of confidence,
    * i.e. the range +- x within which a deviation is considered within tolerance and where l signifies the extent of the PDF.
    *
    * This is based on the inverse Error function (see https://en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function).
    *
    * @param l the standard deviation.
    * @param p the confidence desired for the likelihood.
    * @return the value of x such that p iw the probability of a random number x (with mean 0, and variance 1/2) falling between -x and x.
    */
  def wiggle(l: Double, p: Double): Double = l / sigma * erfInv(1 - p)

  /**
    * The standard deviation of a normals distribution whose variance is 1/2.
    * This is the basis of the inverse error function.
    */
  val sigma: Double = math.sqrt(0.5)
}

/**
  * Trait which models the behavior of something with (maybe) fuzziness.
  *
  * See also related trait Fuzzy[X] but note that there, the parametric type X
  * corresponds to the quantity itself, not its fuzziness.
  *
  * @tparam T the type of fuzziness.
  */
trait Fuzz[T] {
  /**
    * The (optional) fuzziness: if None, then there is no fuzziness.
    */
  val fuzz: Option[Fuzziness[T]]
}

/**
  * Type class trait Valuable[T].
  *
  * This is used to represent quantities of different underlying units/dimensions.
  *
  * @tparam T the underlying type of this Valuable.
  */
trait Valuable[T] extends Fractional[T] {
  /**
    * Method to return a String representation of a T value.
    *
    * @param t a T value.
    * @return the corresponding String.
    */
  def render(t: T): String

  /**
    * Method to yield a T from a Double.
    *
    * @param x a Double.
    * @return the corresponding value of T.
    */
  def fromDouble(x: Double): T

  /**
    * Method to scale a T value, according to a constant.
    *
    * This is essentially the inverse of the ratio method.
    *
    * @param t a T value.
    * @param f a factor (a Double, i.e. dimensionless).
    * @return a scaled value of T.
    */
  def scale(t: T, f: Double): T

  /**
    * Method to yield a "normalized" version of x.
    * For a Numeric object, this implies the absolute value, i.e. with no sign.
    *
    * @param x the value.
    * @return the value, without any sign.
    */
  def normalize(x: T): T

  /**
    * Method to yield the ratio of two T values.
    *
    * This is essentially the inverse of the scale method.
    *
    * @param t1 a T value.
    * @param t2 a T value.
    * @return t1/t2 as a Double.
    */
  def ratio(t1: T, t2: T): Double = toDouble(t1) / toDouble(t2)

  /**
    * Method to multiply a U by a V, resulting in a T.
    *
    * @param u a value of U.
    * @param v a value of V.
    * @tparam U the type of u.
    * @tparam V the type of v.
    * @return a T whose value is u * v.
    */
  def multiply[U: Valuable, V: Valuable](u: U, v: V): T = fromDouble(implicitly[Valuable[U]].toDouble(u) * implicitly[Valuable[V]].toDouble(v))

  /**
    * Method to divide a U by a V, resulting in a T.
    *
    * @param u a value of U.
    * @param v a value of V.
    * @tparam U the type of u.
    * @tparam V the type of v.
    * @return a T whose value is u / v.
    */
  def divide[U: Valuable, V: Valuable](u: U, v: V): T = fromDouble(implicitly[Valuable[U]].toDouble(u) / implicitly[Valuable[V]].toDouble(v))
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
    * @param t a value.
    * @param f a factor.
    * @return x * f.
    */
  def scale(t: Double, f: Double): Double = t * f

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