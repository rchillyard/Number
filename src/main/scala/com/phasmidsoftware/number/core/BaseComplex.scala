package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.BaseComplex.narrow
import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{negate, zero, zeroR}

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
        case c: Complex if c.modulus.isProbablyZero(0.5) => 0
        case c: Complex => c.modulus.compare(Number.zero) // TESTME is this right?
        case _ => throw ComplexException(s"not implemented")
      }
    case y: Number =>
      compare(ComplexCartesian(y))
  }

  /**
    * Method to determine if this Complex can be evaluated exactly.
    *
    * NOTE: the implementations of this don't always make perfect sense regarding maybeFactor.
    *
    * @return true if materialize will result in an exact Field, else false.
    */
  def isExact(maybeFactor: Option[Factor]): Boolean = real.isExact(maybeFactor) && imag.isExact(maybeFactor)

  /**
    *
    * @return true if the magnitude of this Complex is one.
    */
  def isUnity: Boolean = modulus.doSubtract(Number.one).isZero

  /**
    * Add x to this Complex and return the result.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = sum(narrow(x, polar = false))

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field = make(Number.negate(real), Number.negate(imag))

  /**
    * Multiply this Complex by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case n@Number(_, _) => numberProduct(n)
    case c@BaseComplex(_, _) => product(c)
  }

  /**
    * Divide this Complex by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = this multiply x.invert

  /**
    * Raise this Complex to the power p.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Field): Field = p match {
    case r: Real => power(r.x)
    case n: Number =>
      this match {
        case ComplexPolar(re, im, w) if n.isRational => doRationalPowerForComplexPolar(n, re, im, w)
        case ComplexPolar(re, im, w) => ComplexPolar(re.doPower(n), im.doMultiply(n), w)
        case ComplexCartesian(_, _) if n.isInteger => doIntPowerForComplexCartesian(p, n)
        case c@ComplexCartesian(_, _) => convertToPolar(c).power(n)
      }
    case ComplexCartesian(x, y) => ComplexPolar(convertToNumber(power(x)), y.make(NatLog))
    case _ => throw NumberException(s"power not supported for polar complex powers: $this ^ $p")
  }

  private def doIntPowerForComplexCartesian(p: Field, n: Number) = {
    n.toInt match {
      case Some(0) => Complex.unit
      case Some(-1) => this.conjugate divide (modulus doMultiply modulus)
      case Some(1) => this
      case Some(x) if x > 0 => LazyList.continually(this).take(x).toList.foldLeft[Complex](Complex.unit)((a, b) => a doMultiply b)
      case _ => throw ComplexException(s"not implemented: power($p)")
    }
  }

  private def doRationalPowerForComplexPolar(n: Number, re: Number, im: Number, w: Int) = {
    recover(
      for {
        z <- n.toRational
        r <- Literal(re).^(n).evaluate match {
          case x: Number => Some(x.simplify)
          case Real(x) => Some(x.simplify)
          case x => x.asNumber
        }
        branches <- (z.invert * w).maybeInt
      }
      yield
        ComplexPolar(r, im.doMultiple(z), branches),
      ComplexException("logic error: power")
    )
  }

  /**
    * Method to raise this Complex value to the (integer) power p.
    *
    * @param p an Int.
    * @return this Field raised to power p.
    */
  def power(p: Int): Field = p match {
    case 0 => Number.one
    case 1 => this
    case _ => power(Number(p))
  }

  /**
    * Yields the inverse of this Complex.
    *
    * @return the result of invoking power(-1).
    */
  def invert: Field = power(-1)

  /**
    * Method to determine if this Field is actually a real (or imaginary) Number (i.e. not complex).
    * NOTE: to force this as a Number, use convertToNumber in the companion Object.
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number] =
    this match {
      case ComplexCartesian(x, y) if y == Number.zero => Some(x)
      case ComplexCartesian(x, y) if x == Number.zero => Some((y doMultiply y).makeNegative.make(Root2))
      case ComplexPolar(r, theta, _) if theta == Number.zero => Some(r)
      case ComplexPolar(r, theta, _) if theta == Number.pi => Some(r.makeNegative)
      case p@ComplexPolar(_, theta, _) if (theta doMultiply 2).abs == Number.pi => convertToCartesian(p).asNumber
      case _ => None
    }

  /**
    * Method to determine the conjugate of this Complex number.
    *
    * @return the conjugate of this Complex.
    */
  def conjugate: Complex = make(real, imag.makeNegative)

  /**
    * Instance method to make a Complex number from a real and an imaginary part.
    *
    * @param a the real part.
    * @param b the imaginary part.
    * @return a Complex number, either ComplexCartesian or ComplexPolar
    */
  def make(a: Number, b: Number): BaseComplex

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
    * Method to multiply this by the given parameter (a Polar).
    *
    * @param multiplicand the complex multiplicand.
    * @return the product of this and multiplicand.
    */
  def product(multiplicand: Complex): Complex = this match {
    case ComplexPolar(_, _, _) => doMultiply(multiplicand)
    case ComplexCartesian(_, _) => multiplicand match {
      case ComplexCartesian(_, _) => doMultiply(multiplicand)
      case ComplexPolar(_, _, _) => narrow(this, polar = true) doMultiply multiplicand
    }
  }

  /**
    * Method to render the imaginary value as a String.
    *
    * @return a String representing the imaginary value.
    */
  protected def showImaginary(polar: Boolean, branch: Int = 0, n: Int = 1): String = (imag, branch, n) match {
    case (Number.zero, 0, 1) | (Number.zeroR, 0, 1) => "0"
    case (x, 0, 1) =>
      // TODO Try to merge this code with the following case
      val sign = (x, polar) match {
        case (Number.zero, true) => ""
        case (_, true) => ""
        case _ if x.isPositive => "+"
        case _ => "-"
      }
      s"${sign}i${x.abs}"
    case (Number.zeroR, z, n) =>
      val x = Number.zeroR.doAdd(Number.twoPi.doMultiply(Number(z)).doDivide(n))
      val sign = (x, polar) match {
        case (Number.zero, true) => ""
        case (_, true) => ""
        case _ if x.isPositive => "+"
        case _ => "-"
      }
      s"${sign}i${x.abs}"
  }
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
  def unapply(complex: BaseComplex): Option[(Number, Number)] = Some(complex.real, complex.imag)

  /**
    * Method to take a field and narrow it to a BaseComplex.
    *
    * @param x     a Field.
    * @param polar whether we want a polar result or a cartesian result.
    * @return a BaseComplex.
    */
  def narrow(x: Field, polar: Boolean): BaseComplex = x match {
    case c@ComplexCartesian(_, _) => if (polar) convertToPolar(c) else c
    case c@ComplexPolar(_, _, _) => if (!polar) convertToCartesian(c) else c
    case n@Number(_, _) => ComplexCartesian(n, Number.zero)
    case Real(x) => ComplexCartesian(x, Number.zero)
    case _ => throw NumberException(s"BaseComplex: narrow: x can't be matched: $x")
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
    * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
    *
    * @return true is y is zero.
    */
  def isReal: Boolean = y.isZero

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: Number = convertToPolar(this).real

  /**
    *
    * @return a Number (in radians).
    */
  def argument: Number = convertToPolar(this).argument

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = if (real.factor == imag.factor) Some(real.factor) else None

  /**
    * Rotate this Complex number by pi/2 counter-clockwise (i.e. multiply by i).
    *
    * @return the value of this * i.
    */
  def rotate: BaseComplex = ComplexCartesian(imag.makeNegative, real)

  /**
    * Method to multiply this BaseComplex by a Number.
    *
    * @param n the Number.
    * @return a Complex with the same argument as this but a different magnitude.
    */
  def numberProduct(n: Number): Complex =
    if (n.isImaginary) doMultiply(ComplexCartesian.fromImaginary(n))
    else
      make(x doMultiply n, y doMultiply n)

  /**
    * Method to make a BaseComplex from a pair of numbers (treated as the real and imaginary parts of a
    * Cartesian Complex number).
    *
    * @param a the real part.
    * @param b the imaginary part.
    * @return a Complex number, either ComplexCartesian or ComplexPolar
    */
  def make(a: Number, b: Number): BaseComplex = ComplexCartesian(a, b)

  /**
    * Method to determine if this complex number is probably zero (with probability of 1/2).
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean = x.isProbablyZero(0.5) && y.isProbablyZero(0.5)

  /**
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean = x.isInfinite || y.isInfinite

  /**
    * TEST me
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field = ComplexCartesian(convertToNumber(x.normalize), convertToNumber(y.normalize)) match {
    case ComplexCartesian(real, imag) if imag.isZero => real
    case c => c
  }

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = if (isReal) x.toString else s"""($x${showImaginary(polar = false)})"""

  /**
    * Add two Cartesian Complex numbers.
    *
    * @param complex the addend.
    * @return the sum.
    */
  def doAdd(complex: Complex): BaseComplex = complex match {
    case ComplexCartesian(a, b) =>
      import com.phasmidsoftware.number.core.Expression.ExpressionOps
      Complex.apply(Literal(x).+(a).materialize, (Literal(y) + b).materialize, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case c@ComplexPolar(_, _, _) => doAdd(convertToCartesian(c))
  }

  /**
    * Add two Cartesian Complex numbers.
    *
    * @param complex the addend.
    * @return the sum.
    */
  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexCartesian(a, b) =>
      val real: Number = (a doMultiply x) doAdd negate(b doMultiply y)
      val imag: Number = (a doMultiply y) doAdd (b doMultiply x)
      Complex.apply(real, imag, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case ComplexPolar(_, _, _) => throw ComplexException("logic error: ComplexCartesian.doAdd")
  }
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
  def apply(x: Int, y: Int): ComplexCartesian = ComplexCartesian(Number(x), Number(y))

  /**
    * Method to create a real-valued ComplexCartesian.
    *
    * @param x the real value.
    * @return a ComplexCartesian with values x and 0.
    */
  def apply(x: Number): ComplexCartesian = ComplexCartesian(x, 0)

  def fromImaginary(number: Number): Complex = number match {
    case Number(v, Root2) if Value.signum(v) < 0 => ComplexCartesian(zero, Number.create(Value.abs(v)))
    case _ => throw ComplexException(s"fromImaginary: logic error for $number")
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

  if (theta.factor != Radian)
    println(s"polar theta is not in radians: $this")

  /**
    * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if the angle theta is a multiple of pi.
    */
  def isReal: Boolean = theta.doDivide(Number.pi).isInteger

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
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor of r) if factor of theta is Radian; otherwise None.
    */
  def maybeFactor: Option[Factor] = if (real.factor == imag.factor) Some(real.factor) else None

  /**
    * Rotate this Complex number by pi/2 counter-clockwise (i.e. multiply by i).
    *
    * @return the value of this * i.
    */
  def rotate: BaseComplex = ComplexPolar(real, imag doAdd Number.piBy2)

  def numberProduct(n: Number): Complex = {
    // TODO this first option currently works only for i, not for multiples of i.
    if (n.isImaginary) convertToCartesian(this).rotate
    else make(r doMultiply n, theta)
  }

  def make(a: Number, b: Number): BaseComplex = ComplexPolar(a, b.modulate)

  def isZero: Boolean = r.isZero

  def isInfinite: Boolean = r.isInfinite

  def normalize: Field = (r, Number.modulate(theta), n) match {
    case (z, ExactNumber(Value(0), Radian), 1) => z
    case (z, ExactNumber(Value(1), Radian), 1) => -z
    case (z, t, n) => ComplexPolar(z, t, n)
  }

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * NOTE that some of these special cases that are handled here should be eliminated by a prior call to normalize.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = (r, theta, n) match {
    case (Number.one, Number.zero, 1) => "1"
    case (Number.one, Number.pi, 1) => "-1"
    case (_, _, 2) => theta.value match {
      case Value(0) | Value(_, Rational.zero) | Value(_, _, 0.0) => "\u00b1" + r
      case _ => s"${r}e^${showImaginary(polar = true)}"
    }
    case (_, _, 3) => theta.value match {
      case Value(0) | Value(_, Rational.zero) | Value(_, _, 0.0) => s"{$r, ±${r}e^${showImaginary(polar = true, 1, 3)}}"
      case _ => s"${r}e^${showImaginary(polar = true)}"
    }
    // TODO handle the case where n is greater than 2
    case _ =>
      val w = showImaginary(polar = true)
      if (w == "0") r.toString else s"${r}e^$w"
  }

  override def toString: String = render

  def doAdd(complex: Complex): Complex = convertToCartesian(this).doAdd(complex)

  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexPolar(a, b, _) => make(r doMultiply a, theta doAdd b)
    case _ => throw ComplexException("logic error: ComplexPolar.doMultiply")
  }
}

object ComplexPolar {
  def apply(r: Number, theta: Number, n: Int): ComplexPolar = new ComplexPolar(r, theta, n)

  def apply(r: Number, theta: Number): ComplexPolar = apply(r, theta, 1)

  def apply(r: Number): ComplexPolar = apply(r, Number.zeroR)

  def apply(r: Int, theta: Number): ComplexPolar = apply(Number(r), theta)

  /**
    * Method to create a ComplexPolar object with two branches.
    *
    * @param x a Number representing the real part.
    * @return a ComplexPolar of magnitude x, and two points along the x-axis the 0 mid-way between.
    */
//noinspection NonAsciiCharacters
def ±(x: Number): Field = ComplexPolar(x, zeroR, 2)
}

case class ComplexException(str: String) extends Exception(str)