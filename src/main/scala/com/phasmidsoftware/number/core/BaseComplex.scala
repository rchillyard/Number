/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.BaseComplex.narrow
import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{negate, two, zero, zeroR}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.expression.Literal
import com.phasmidsoftware.number.misc.FP.recover

/**
 * Abstract base class which implements Complex.
 *
 * @param real the real component.
 * @param imag the imaginary component.
 */
abstract class BaseComplex(val real: Number, val imag: Number) extends Complex {

  /**
   * Method to return this Field as a Complex.
   * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
   *
   * @return a Complex.
   */
  def asComplex: Complex = this

  /**
   * Method to compare this BaseComplex with that Field.
   * Required by implementing Ordered[Field].
   * NOTE if the difference is a Complex number, we try to do fuzzy comparison (with confidence of 0.5).
   *
   * @param that (a Field).
   * @return the comparison.
   */
  def compare(that: Field): Int = that match {
    case y: Complex =>
      val difference = this + -y
      if (difference.isZero) 0
      else difference match {
        case c: Complex if c.modulus.isProbablyZero() =>
          0
        case c: Complex =>
          c.modulus.compareTo(Number.zero) // TESTME is this right?
        case _ =>
          throw ComplexException(s"not implemented")
      }
    case Real(y) =>
      compare(ComplexCartesian(y))
  }

  /**
   * Method to determine if this Field is equivalent to another Field (x).
   *
   * @param x the other field.
   * @return true if they are the same, otherwise false.
   */
  def isSame(x: Numerical): Boolean = (this, x) match {
    case (z, Real(n)) =>
      z isSame n.asComplex
    case (c1@ComplexCartesian(_, _), c2@ComplexPolar(_, _, _)) =>
      c2.isSame(c1)
    case (c1@ComplexPolar(_, _, _), c2@ComplexCartesian(_, _)) =>
      c1.isSame(convertToPolar(c2))
    case (c1, c2: Complex) =>
      (c1 subtract c2).isZero
    case (z, x: Number) =>
      z isSame Real(x)
  }

  /**
   * Determines if this complex number has a modulus of unity (magnitude of one).
   * This is true if the modulus of the complex number, when subtracted by one, equals zero.
   * TESTME
   *
   * @return true if the modulus of this complex number is unity (1), otherwise false.
   */
  def isUnity: Boolean =
    modulus.doSubtract(Number.one).isZero

  /**
   * Add x to this Complex and return the result.
   *
   * @param x the addend.
   * @return the sum.
   */
  def add(x: Field): Field =
    sum(narrow(x, polar = false))

  /**
   * Method to add this to the given parameter (a Cartesian).
   *
   * @param addend the complex addend.
   * @return the sum of this and addend.
   */
  def sum(addend: Complex): Complex = this match {
    case ComplexCartesian(_, _) => doAdd(addend)
    case ComplexPolar(_, _, _) => narrow(this, polar = false) doAdd addend
  }

  /**
   * Divide this Complex by x and return the result.
   * See * and invert for more detail.
   *
   * @param x the divisor.
   * @return the quotient.
   */
  def divide(x: Field): Field =
    this multiply x.invert

  /**
   * Multiply this Complex by x and return the result.
   *
   * * @param x the multiplicand.
   * * @return the product.
   */
  def multiply(x: Field): Field = x match {
    case Real(n) => numberProduct(n)
    case c@BaseComplex(_, _) => product(c)
  }

  /**
   * Method to multiply this by the given parameter (a Polar).
   *
   * @param multiplicand the complex multiplicand.
   * @return the product of this and multiplicand.
   */
  def product(multiplicand: Complex): Complex = (this, multiplicand) match {
    case (ComplexPolar(_, _, _), ComplexPolar(_, _, _)) =>
      doMultiply(multiplicand)
    case (ComplexPolar(_, _, _), ComplexCartesian(_, _)) =>
      narrow(this, polar = false) doMultiply multiplicand
    case (ComplexCartesian(_, _), ComplexCartesian(_, _)) =>
      doMultiply(multiplicand)
    case (ComplexCartesian(_, _), ComplexPolar(_, _, _)) =>
      narrow(this, polar = true) doMultiply multiplicand
  }

  /**
    * Computes the result of raising this number (field element) to the given power `n`.
    *
    * @param n The exponent as a `Number` to which this field element will be raised.
    *          Can represent integer, rational, or other numerical values.
    * @return A new `Field` instance that is the result of performing the power operation.
    */
  def power(n: Number): Complex = this match {
    case ComplexPolar(re, im, w) if n.isRational =>
      doRationalPowerForComplexPolar(n, re, im, w)
    case ComplexPolar(re, im, w) =>
      ComplexPolar(re.doPower(n), im.doMultiply(n), w)
    case c@ComplexCartesian(_, _) => n match {
      case Number.one =>
        c
      case Number.negOne =>
        c.invert
      case Number.two =>
        c.square
      case _ =>
        convertToPolar(c).power(n).asInstanceOf[Complex] // CONSIDER try to improve upon this
    }
  }

  /**
   * Raise this Complex to the power p.
   *
   * @param p a Number.
   * @return this Number raised to power p.
   */
  def power(p: Field): Field = (this, p) match {
    case (_, Constants.zero) =>
      Constants.one
    case (_, Constants.one) =>
      this
    case (_, Constants.minusOne) =>
      invert
    case (_, Constants.two) =>
      square
    case (ComplexPolar(Number.e, Number.zeroR, _), ComplexCartesian(Number.zero, Number.pi)) =>
      Constants.minusOne
    case (ComplexCartesian(_, Number.zeroR), x) =>
      power(x)
    case _ =>
      throw NumberException(s"power not supported for: $this ^ $p")
  }

  /**
    * Method to compute the square of the modulus (magnitude) of this complex number.
    *
    * @return the square of the modulus as a Number.
    */
  def modulusSquared: Number

  /**
   * Yields the inverse of this Complex.
   *
   * @return the result of invoking power(-1).
   */
  def invert: Field

  /**
   * Method to determine if this Field is actually a real (or imaginary) Number (i.e. not complex).
   * NOTE: to force this as a Number, use convertToNumber in the companion Object.
   * TESTME (partial)
   *
   * @return a Some(x) if this is a Number; otherwise return None.
   */
  def asNumber: Option[Number] =
    this match {
      case ComplexCartesian(x, y) if y.isProbablyZero() =>
        Some(x)
      case ComplexCartesian(x, y) if x.isProbablyZero() =>
        Some((y doMultiply y).makeNegative.make(SquareRoot))
      case ComplexPolar(r, theta, _) if theta.isProbablyZero() =>
        Some(r)
      // CONSIDER allowing approximately pi
      case ComplexPolar(r, theta, _) if theta == Number.pi =>
        Some(r.makeNegative)
      case p@ComplexPolar(_, theta, _) if (theta doMultiply 2).abs == Number.pi =>
        convertToCartesian(p).asNumber
      case _ =>
        None
    }

  /**
   * Method to determine the conjugate of this Complex number.
   *
   * @return the conjugate of this Complex.
   */
  def conjugate: Complex =
    make(real, imag.makeNegative)

  /**
   * Instance method to make a Complex number from a real and an imaginary part.
   *
   * @param a the real part.
   * @param b the imaginary part.
   * @return a Complex number, either ComplexCartesian or ComplexPolar
   */
  def make(a: Number, b: Number): BaseComplex

  /**
    * Computes the sine of this complex number.
    * The result is calculated in the context of complex numbers, applying the necessary
    * formulas for the sine function on complex inputs.
    *
    * @return the sine of this complex number as an instance of `Field`.
    */
  def sin: Field = ??? // TODO implement me

  /**
    * Computes the cosine of this field element, interpreted in the context of a complex number.
    * The cosine operation is defined mathematically and extended to work with complex values,
    * leveraging the properties of exponential functions in the complex plane.
    *
    * @return a `Field` representing the cosine of this field element.
    */
  def cos: Field = ??? // TODO implement me

  /**
    * Computes the tangent of this complex number.
    *
    * @return The tangent of this complex number as a `Field`.
    */
  def tan: Field = ??? // TODO implement me

  /**
    * Computes the arc tangent of the given real number `y` in the context of this `Field`.
    *
    * @param y the real number whose arc tangent is to be computed.
    * @return a `Field` representing the arc tangent of the input.
    */
  def atan(y: Real): Field = ??? // TODO implement me

  /**
    * Computes the natural logarithm of this Field.
    * For a real number x, this is equivalent to the logarithm base e: ln(x).
    * For a complex number, this computes the multi-valued complex logarithm.
    *
    * @return the natural logarithm of this Field as a Field.
    */
  def ln: Field

  /**
    * Computes the exponential of this complex number.
    *
    * @return the exponential of this Field as a new Field instance.
    */
  def exp: Field = ??? // TODO implement me

  /**
   * Method to render the imaginary value as a String.
   *
   * @return a String representing the imaginary value.
   */
  protected def showImaginary(polar: Boolean, branch: Int = 0, n: Int = 1): String = (imag, branch, n) match {
    case (Number.zero, 0, 1) | (Number.zeroR, 0, 1) =>
      "0"
    case (x, 0, 1) => // TESTME
      // CONSIDER Try to merge this code with the following case
      val sign = (x, polar) match {
        case (Number.zero, true) =>
          ""
        case (_, true) if x.isPositive =>
          ""
        case _ if x.isPositive =>
          "+"
        case _ =>
          "-"
      }
      s"${sign}i${x.abs.render}"
    case (Number.zeroR, z, n) =>
      val x = Number.zeroR.doAdd(Number.twoPi.doMultiply(z).doDivide(n))
      val sign = (x, polar) match {
        case (Number.zero, true) =>
          ""
        case (_, true) =>
          ""
        case _ if x.isPositive =>
          "+"
        case _ =>
          "-"
      }
      s"${sign}i${x.abs.render}"
  }

  /**
    * Method to compute the result of raising a complex number, represented in polar form,
    * to a rational power. This involves converting the given power to a rational, computing
    * the radial component raised to the power, and resolving the appropriate branch for the
    * result based on the total number of branches `w`.
    *
    * @param n  the power to which the complex number is raised.
    * @param re the radial component (magnitude) of the complex number in polar form.
    * @param im the angular component (argument) of the complex number in polar form.
    * @param w  the number of branches to consider for the result, typically used for
    *           handling multi-valued functions like roots in the complex plane.
    */
  private def doRationalPowerForComplexPolar(n: Number, re: Number, im: Number, w: Int) = recover(
    for {
      z <- n.toNominalRational
      r = re power n
      branches <- (z.invert * w).maybeInt
    } yield ComplexPolar(r, im.doMultiple(z), branches),
    ComplexException("logic error: power")
  )
}

/**
 * Companion object to BaseComplex.
 */
object BaseComplex {

  /**
   * Method used by pattern-matching to yield the real and imaginary parts of a BaseComplex.
   *
   * NOTE this is dangerous.
   *
   * @param complex a BaseComplex.
   * @return an optional tuple of two Numbers (real, imag).
   */
  def unapply(complex: BaseComplex): Option[(Number, Number)] =
    Some(complex.real, complex.imag)

  /**
   * Method to take a field and narrow it to a BaseComplex.
   *
   * @param x     a Field.
   * @param polar whether we want a polar result or a cartesian result.
   * @return a BaseComplex.
   */
  def narrow(x: Field, polar: Boolean): Complex = x match {
    case c@ComplexCartesian(_, _) =>
      if (polar) convertToPolar(c) else c
    case c@ComplexPolar(_, _, _) =>
      if (!polar) convertToCartesian(c) else c
    case Real(x) =>
      ComplexCartesian(x, Number.zero)
    case _ =>
      throw NumberException(s"BaseComplex: narrow: x can't be matched: $x")
  }
}

/**
 * Case class to represent a Cartesian complex object.
 *
 * @param x the real part.
 * @param y the imaginary part.
 */
case class ComplexCartesian(x: Number, y: Number) extends BaseComplex(x, y) {

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    x.isExact && y.isExact

  /**
   * Method to determine the modulus of this Complex number.
   *
   * CONSIDER implementing real in the Complex trait (not just BaseComplex).
   *
   * @return the modulus of this Complex.
   */
  def modulus: Number =
    if (isReal) real else if (isImaginary) imag else convertToPolar(this).asInstanceOf[BaseComplex].real

  /**
   * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
   *
   * @return true is y is zero.
   */
  def isReal: Boolean =
    y.isZero

  /**
   * Method to determine if this Complex is imaginary-valued (i.e. the point lies on the imaginary axis).
   * TESTME
   *
   * @return true if the real part is zero (argument is a multiple of pi/2).
   */
  def isImaginary: Boolean =
    x.isZero

  /**
   * Change the sign of this Number.
   */
  def unary_- : Field =
    make(Number.negate(real), Number.negate(imag))

  /**
   *
   * @return a Number (in radians).
   */
  def argument: Number =
    convertToPolar(this).argument

  /**
   * Rotate this Complex number by pi/2 counter-clockwise (i.e. multiply by i).
   * TESTME
   *
   * @return the value of this * i.
   */
  def rotate: BaseComplex =
    ComplexCartesian(imag.makeNegative, real)

  /**
   * Method to multiply this BaseComplex by a Number.
   *
   * @param n the Number.
   * @return a Complex with the same argument as this but a different magnitude.
   */
  def numberProduct(n: Number): Complex =
    if (n.isImaginary)
      doMultiply(ComplexCartesian.fromImaginary(n))
    else
      make(x doMultiply n, y doMultiply n)

  /**
   * Add two Cartesian Complex numbers.
   * TESTME ?
   *
   * @param complex the addend.
   * @return the sum.
   */
  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexCartesian(a, b) =>
      val real: Number = (a doMultiply x) doAdd (b doMultiply y doMultiply Number.negOne)
      val imag: Number = (a doMultiply y) doAdd (b doMultiply x)
      ComplexCartesian(real, imag)
    case ComplexPolar(_, _, _) =>
      throw ComplexException("logic error: ComplexCartesian.doAdd")
  }

  /**
   * Method to determine if this complex number is probably zero (with probability of 1/2).
   * TESTME
   *
   * @return true if the magnitude of this Field is zero.
   */
  def isZero: Boolean =
    x.isProbablyZero() && y.isProbablyZero()

  /**
   * Determines if either the real or imaginary part of this Complex number is infinite.
   * TESTME
   *
   * @return true if the real part or the imaginary part is infinite; false otherwise.
   */
  def isInfinite: Boolean =
    x.isInfinite || y.isInfinite

  /**
    * Computes the natural logarithm of this Field.
    * For a real number x, this is equivalent to the logarithm base e: ln(x).
    * For a complex number, this computes the multi-valued complex logarithm.
    *
    * @return the natural logarithm of this Field as a Field.
    */
  def ln: Field = throw ComplexException("not implemented: ComplexCartesian.ln")

  /**
   * TESTME
   *
   * @return a Field which is in canonical form.
   */
  def normalize: Field =
    ComplexCartesian(convertToNumber(x.normalize), convertToNumber(y.normalize)) match {
      case ComplexCartesian(real, imag) if imag.isZero =>
        Real(real)
      case c =>
        c
  }

  /**
    * Action to simplifyAndEvaluate this Expression and render it as a String,
   * that is to say we eagerly evaluate this Expression as a String.
   *
   * @return a String representing the value of this expression.
   */
  def render: String =
    if (isReal)
      x.render
    else if (isImaginary)
      s"i${y.render}"
    else
      s"""(${x.render}${showImaginary(polar = false)})"""

  /**
   * Add two Cartesian Complex numbers.
   *
   * @param complex the addend.
   * @return the sum.
   */
  def doAdd(complex: Complex): BaseComplex = complex match {
    case ComplexCartesian(a, b) =>
      // CONSIDER we should not be relying on expression package here.
      import com.phasmidsoftware.number.expression.Expression.ExpressionOps
      // TODO replace materialize with evaluateAsIs and do the appropriate things below (a for-comprehension).
      val partA = Literal(x).+(Real(a)).materialize
      val partB = Literal(y).+(Real(b)).materialize
      Complex.apply(partA, partB, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case c@ComplexPolar(_, _, _) =>
      doAdd(convertToCartesian(c))
  }

  /**
    * Computes the square of a complex number represented in Cartesian form.
    * The result is a new `ComplexCartesian` value obtained by applying the formula:
    * (x^2 - y^2, 2xy), where `x` and `y` are the real and imaginary parts respectively.
    *
    * @return a `ComplexCartesian` instance representing the square of the original complex number.
    */
  def square: ComplexCartesian =
    ComplexCartesian(x.doPower(Number.two) doSubtract y.doPower(Number.two), x doMultiply y doMultiply Number.two)

  /**
   * Yields the inverse of this Complex.
   *
   * @return the result of invoking power(-1).
   */
  def invert: Complex =
    conjugate.asInstanceOf[ComplexCartesian] scale modulusSquared.getInverse

  /**
    * Calculates the square of the modulus of the complex number represented by this instance.
    * The modulus squared is computed as the sum of the squares of the real and imaginary components.
    *
    * @return the squared modulus of the complex number.
    */
  def modulusSquared: Number =
    imag.doPower(two) doAdd real.doPower(two)

  /**
   * Method to scale this Cartesian Complex number by a factor.
   *
   * TODO implement for more imaginary factors.
   *
   * @param x the factor (a Number).
   * @return this scaled by x.
   */
  private def scale(x: Number) = x match {
    case Number.i =>
      make(negate(imag), real)
    case _ =>
      make(real doMultiply x, imag doMultiply x)
  }

  /**
   * Method to make a BaseComplex from a pair of numbers (treated as the real and imaginary parts of a
   * Cartesian Complex number).
   *
   * @param a the real part.
   * @param b the imaginary part.
   * @return a Complex number, either ComplexCartesian or ComplexPolar
   */
  def make(a: Number, b: Number): BaseComplex =
    ComplexCartesian(a, b)

  /**
   * Method to return this Complex as a Real, if possible.
   * If this is a Real number x, return Some(x) otherwise, return None.
   *
   * @return an Option[Real].
   */
  def asReal: Option[Real] =
    if (isReal) Some(Real(x)) else None

  /**
   * Determine the "sign" of this field.
   * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
   * For a complex number, we get the signum of the real part.
   *
   * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
   */
  def signum: Int =
    x.signum

  /**
    * Computes the absolute value of this complex number, represented in Cartesian coordinates.
    * The result is a new complex number with both the real and imaginary components replaced
    * by their respective absolute values.
    *
    * NOTE WARNING this is very arbitrary. Maybe abs should not be in NumberLike.
    *
    * @return a Numerical instance that is the absolute value of this complex number.
    */
  def abs: Numerical = ComplexCartesian(x.abs, y.abs)
}

/**
 * Companion object to ComplexCartesian.
 */
object ComplexCartesian {

  /**
   * Method to create a ComplexCartesian from two Int parameters.
   *
   * @param x the real part.
   * @param y the imaginary part.
   * @return a ComplexCartesian made of up x and y.
   */
  def apply(x: Int, y: Int): ComplexCartesian =
    ComplexCartesian(Number(x), Number(y))

  /**
   * Method to create a real-valued ComplexCartesian.
   * TESTME
   *
   * @param x the real value.
   * @return a ComplexCartesian with values x and 0.
   */
  def apply(x: Number): ComplexCartesian =
    ComplexCartesian(x, 0)

  /**
   * Constructs a complex number from an imaginary part.
   *
   * @param number the imaginary part represented as a Number instance. It must meet specific
    *              criteria (e.g., be associated with SquareRoot and have a negative value).
   * @return a Complex number in Cartesian form with a real part of zero and the provided imaginary part.
   * @throws ComplexException if the given number does not satisfy the required logic.
   */
  def fromImaginary(number: Number): Complex = number match {
    case Number(v, SquareRoot) if Value.signum(v) < 0 =>
      ComplexCartesian(zero, Number.create(Value.abs(v)))
    case _ =>
      throw ComplexException(s"fromImaginary: logic error for $number")
  }
}

/**
 * Class to represent a family of Complex numbers in polar form.
 * An instance of this class actually represents n different complex numbers.
 *
 * @param r     the real part (the magnitude) of the number(s).
 * @param theta the imaginary part (the angle) of the 0th branch of the number.
 * @param n     the number of "branches" in this set of complex numbers.
 */
case class ComplexPolar(r: Number, theta: Number, n: Int = 1) extends BaseComplex(r, theta) {
  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    r.isExact && theta.isExact

  require(theta.factor == Radian, "polar theta is not in radians")

  //  require(!r.isZero, "polar radius is zero")
  if (r.isZero)
    println(s"Warning: Polar r is zero: $this") // TODO make this a requirement

  /**
   * Method to determine if this Complex is imaginary-valued (i.e. the point lies on the imaginary axis).
   *
   * @return true if the real part is zero (argument is a multiple of pi/2).
   */
  def isImaginary: Boolean =
    rotate.isReal

  /**
   * Rotate this Complex number by pi/2 counter-clockwise (i.e. multiply by i).
   *
   * @return the value of this * i.
   */
  def rotate: ComplexPolar =
    rotate(Number.piBy2)

  /**
   * Method to determine the modulus of this Complex number.
   *
   * @return the modulus of this Complex.
   */
  def modulus: Number = r

  /**
   *
   * @return a Number (in radians).
   */
  def argument: Number = theta

  /**
    * Computes the square of the modulus (magnitude) of this ComplexPolar instance.
    *
    * @return the squared modulus as a Number.
    */
  def modulusSquared: Number = r power two

  /**
    * Computes the square of this ComplexPolar instance by doubling its argument (angle).
    * The operation preserves the modulus while doubling the angle in polar representation.
    *
    * @return a new Field representing the squared value of the ComplexPolar instance.
    */
  def square: Field = copy(r = r power 2, theta = theta doMultiple 2)

  /**
   * Yields the inverse of this Complex.
   * TESTME
   *
   * @return the result of invoking power(-1).
   */
  def invert: Field =
    ComplexPolar(real.getInverse, imag.makeNegative)

  /**
   * Change the sign of this Number.
   */
  def unary_- : Field =
    rotate(Number.pi)

  /**
   * Rotate this Complex number by phi counter-clockwise.
   *
   * @return the value of this, rotated by phi.
   */
  def rotate(phi: Number): ComplexPolar =
    ComplexPolar(real, imag doAdd phi)

  /**
    * Computes the natural logarithm of this Field.
    * For a real number x, this is equivalent to the logarithm base e: ln(x).
    * For a complex number, this computes the multi-valued complex logarithm.
    *
    * @return the natural logarithm of this Field as a Field.
    */
  def ln: Field =
    r.ln match {
      case Real(n) =>
        ComplexCartesian(n, theta)
      case _ =>
        throw ComplexException("logic error: ComplexPolar.ln")
    }

  /**
    * Multiplies this ComplexPolar instance with a given Number and returns the result as a Complex.
    * If the given Number is imaginary, the operation involves converting this ComplexPolar to its Cartesian form
    * and applying a rotation. Otherwise, the product is computed in polar form using the magnitude and angle.
    *
    * @param n the Number to multiply with. This can be a real or imaginary number.
    * @return the product as a Complex.
    */
  def numberProduct(n: Number): Complex = {
    // TODO this first option currently works only for i, not for multiples of i.
    if (n.isImaginary) convertToCartesian(this).rotate
    else make(r doMultiply n, theta)
  }

  /**
   * Method to determine if this ComplexPolar is zero.
   *
   * TESTME
   *
   * @return true if the magnitude of this Field is zero.
   */
  def isZero: Boolean =
    r.isZero

  /**
   * Determines if the magnitude of this Complex field is infinite.
   *
   * @return true if the magnitude of this Complex field is infinite, otherwise false.
   */
  def isInfinite: Boolean =
    r.isInfinite

  /**
   * Normalize a ComplexField based on specific rules of theta modulation and context.
   * - If theta is 0 and n is 1, return the real part of `r`.
   * - If theta is 1 (normalized as Radian) and n is 1, return the negation of the real part of `r`.
   * - Otherwise, return a ComplexPolar representation of the field.
   *
   * @return a Field that represents the normalized form of the Instance.
   */
  def normalize: Field = (r, Number.modulate(theta), n) match {
    case (z, ExactNumber(Value(0), Radian), 1) =>
      Real(z)
    case (z, ExactNumber(Value(1), Radian), 1) =>
      -z
    case (z, t, n) =>
      ComplexPolar(z, t, n)
  }

  /**
    * Action to simplifyAndEvaluate this Expression and render it as a String,
   * that is to say we eagerly evaluate this Expression as a String.
   * TESTME (partial)
   *
   * NOTE that some of these special cases that are handled here should be eliminated by a prior call to normalize.
   *
   * @return a String representing the value of this expression.
   */
  def render: String = (r, theta, n) match {
    case (Number.one, Number.zero, 1) => "1"
    case (Number.one, Number.pi, 1) => "-1"
    case (Number.one, Number.minusPi, 1) => "-1"
    // TODO combine these cases that all require rAsString
    case (_, _, 2) =>
      val rAsString = r.render
      theta.nominalValue match {
        case Value(0) | Value(_, Rational.zero) | Value(_, _, 0.0) =>
          "\u00b1" + rAsString // +-
        case _ =>
          s"${rAsString}e^${showImaginary(polar = true)}"
    }
    case (_, _, 3) =>
      val rAsString = r.render
      theta.nominalValue match {
        case Value(0) | Value(_, Rational.zero) | Value(_, _, 0.0) =>
          s"{$rAsString, ±${rAsString}e^${showImaginary(polar = true, 1, 3)}}"
        case _ =>
          s"${rAsString}e^${showImaginary(polar = true)}"
    }
    // TODO handle the case where n is greater than 2
    case _ =>
      val rAsString = r.render
      val w = showImaginary(polar = true)
      if (w == "0") rAsString else s"${rAsString}e^$w"
  }

  /**
   * Adds this ComplexPolar instance to another Complex.
   * The operation is performed by converting this ComplexPolar instance to its Cartesian representation
   * and then delegating to the `doAdd` method of the Cartesian representation.
   *
   * @param complex the other Complex to be added.
   * @return the result of adding this ComplexPolar to the given Complex.
   */
  def doAdd(complex: Complex): Complex =
    convertToCartesian(this).doAdd(complex)

  /**
   * Multiplies this ComplexPolar instance with another Complex.
   * If the input is of type ComplexPolar, the multiplication is performed
   * in polar form using the magnitude and phase (angle).
   *
   * @param complex the other Complex to multiply with.
   * @return the product as a Complex.
   * @throws ComplexException if the given Complex is not a ComplexPolar instance.
   */
  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexPolar(a, b, _) =>
      make(r doMultiply a, theta doAdd b)
    case _ =>
      throw ComplexException("logic error: ComplexPolar.doMultiply")
  }

  /**
   * Constructs a new ComplexPolar instance using the given parameters.
   *
   * @param a the first parameter, generally representing the modulus (magnitude) of the complex number
   * @param b the second parameter, generally representing the argument (angle) of the complex number in radians
   * @return a BaseComplex instance, represented as a ComplexPolar
   */
  def make(a: Number, b: Number): BaseComplex =
    ComplexPolar(a, b)

  /**
   * Method to return this Field as a Real, if possible.
   * If this is a Real number x, return Some(x) otherwise, return None.
   *
   * TESTME
   *
   * @return an Option[Real].
   */
  def asReal: Option[Real] =
    if (isReal) convertToCartesian(this).asReal else None

  /**
   * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
   * TODO fix this properly. We can only really answer this question knowing with branch we're talking about.
   *
   * @return true if the angle theta is a multiple of pi.
   */
  def isReal: Boolean =
    theta.isZero || theta.doDivide(Number.pi).isInteger

  /**
   * Determine the "sign" of this field.
   * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
   * For a complex number, we get the signum of the real part.
   *
   * TESTME
   *
   * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
   */
  def signum: Int =
    convertToCartesian(this).signum

  /**
    * Computes the absolute value (magnitude) of this ComplexPolar instance.
    * The calculation is performed using the Cartesian representation of the complex number.
    * NOTE very arbitrary.
    *
    * @return the absolute value as a Numerical.
    */
  def abs: Numerical = convertToCartesian(this).abs
}

/**
 * Companion object for the ComplexPolar class.
 * Provides factory methods for creating instances of ComplexPolar with various parameters.
 */
object ComplexPolar {
  /**
   * Creates a ComplexPolar object based on the provided magnitude.
    * If the input number has a root (NthRoot), it calculates using the root and sets a default angle.
   * Otherwise, it uses zero as the angle.
   *
   * @param r a Number representing the magnitude of the ComplexPolar object
   * @return a ComplexPolar object with the specified magnitude and calculated or default angle
   */
  def apply(r: Number): ComplexPolar = r match {
    case Number(_, NthRoot(n)) =>
      apply(r, Number.zeroR, n)
    case _ =>
      apply(r, Number.zeroR)
  }

  /**
   * Creates a ComplexPolar object from an integer magnitude and a specified angle.
   * TESTME
   *
   * @param r     an integer representing the radius (magnitude) of the ComplexPolar object
   * @param theta a Number representing the angle (theta) of the ComplexPolar object
   * @return a ComplexPolar object with the specified magnitude and angle
   */
  def apply(r: Int, theta: Number): ComplexPolar =
    apply(Number(r), theta)

  /**
   * Factory method to create a ComplexPolar instance using the specified magnitude and angle.
   *
   * @param r     the magnitude of the ComplexPolar object, represented as a Number
   * @param theta the angle of the ComplexPolar object, represented as a Number
   * @return a ComplexPolar instance with the specified magnitude and angle, and a default branch count of 1
   */
  def apply(r: Number, theta: Number): ComplexPolar =
    apply(r, theta, 1)

  /**
   * Constructs a ComplexPolar object based on the given magnitude, angle, and root count.
   * Adjusts or modulates the angle as necessary based on its type and value.
   *
   * @param r     the magnitude of the ComplexPolar object, represented as a Number
   * @param theta the angle of the ComplexPolar object, represented as a Number in radians
   * @param n     the root count, specifying the number of branches; typically an integer greater than or equal to 1
   * @return a ComplexPolar object with the specified magnitude, angle (modulated if necessary), and root count
   * @throws NumberException if the provided angle does not match the expected format or type
   */
  def apply(r: Number, theta: Number, n: Int): ComplexPolar = theta match {
    case ExactNumber(x, Radian) if Value.signum(x) == 0 =>
      new ComplexPolar(r, zeroR, n)
    case ExactNumber(_, Radian) =>
      new ComplexPolar(r, theta.modulate, n)
    case Number(_, Radian) =>
      new ComplexPolar(r, theta.modulate, n)
    case _ =>
      throw NumberException(s"no match for $theta")
  }

  /**
   * Method to create a ComplexPolar object with two branches.
   *
   * @param x a Number representing the real part.
   * @return a ComplexPolar of magnitude x, and two points along the x-axis the 0 mid-way between.
   */
  def ±(x: Number): Field =
    ComplexPolar(x, zeroR, 2)
}

/**
  * A case class representing a custom exception type called ComplexException.
  *
  * This exception is initialized with a string message that describes the error.
  *
  * @param str The message associated with this exception.
  */
case class ComplexException(str: String) extends Exception(str)