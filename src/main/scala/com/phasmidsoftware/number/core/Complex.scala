package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{convertToCartesian, narrow}
import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.core.GeneralNumber.negate

abstract class Complex(val real: GeneralNumber, val imag: GeneralNumber) extends AtomicExpression with Field {
  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: GeneralNumber

  /**
    * Method to determine the complement of this Complex number.
    *
    * @return the complement of this Complex.
    */
  def complement: Complex = make(real, imag.makeNegative)

  /**
    * Instance method to make a Complex number from a real and an imaginary part.
    *
    * @param a the real part.
    * @param b the imaginary part.
    * @return a Complex number, either ComplexCartesian or ComplexPolar
    */
  def make(a: GeneralNumber, b: GeneralNumber): Complex

  /**
    * Method to determine if this Complex can be evaluated exactly.
    *
    * NOTE: the implementations of this don't always make perfect sense regarding maybeFactor.
    *
    * @return true if materialize will result in an exact Field, else false.
    */
  def isExact: Boolean = real.isExact && imag.isExact

  /**
    * Action to materialize this Complex as a Field,
    *
    * @return this.
    */
  def materialize: Field = this

  /**
    * Add x to this Complex and return the result.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = sum(narrow(x, polar = false))

  /**
    * Change the sign of this GeneralNumber.
    */
  def unary_- : Field = make(real, negate(imag))

  def numberProduct(n: GeneralNumber): Complex

  /**
    * Multiply this Complex by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case n@GeneralNumber(_, _) => numberProduct(n)
    case c@Complex(_, _) => product(c)
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
    * @param p a GeneralNumber.
    * @return this GeneralNumber raised to power p.
    */
  def power(p: GeneralNumber): Field = this match {
    case ComplexPolar(_, _) =>
      recover(
        for (r <- real.^(p).materialize.asNumber; i <- (imag * p).materialize.asNumber) yield make(r, i),
        ComplexException("logic error: power")
      )
    case ComplexCartesian(_, _) => p.toInt match {
      case Some(0) => ComplexCartesian(GeneralNumber.one, GeneralNumber.zero)
      case Some(-1) => -this divide this.magnitudeSquared.materialize
      case Some(x) if x > 0 => LazyList.continually(this).take(x).toList.reduce[Expression]((a, b) => a * b).materialize
      case _ => throw ComplexException(s"not implemented: power($p)")
    }
  }

  def power(p: Int): Field = power(GeneralNumber(p))

  /**
    * Yields the inverse of this Complex.
    */
  def invert: Field = power(-1)

  /**
    * Method to add this to the given parameter (a Cartesian).
    *
    * @param addend the complex addend.
    * @return the sum of this and addend.
    */
  def sum(addend: Complex): Complex = this match {
    case ComplexCartesian(_, _) => doAdd(addend)
    case ComplexPolar(_, _) => narrow(this, polar = false) doAdd addend
  }

  def doAdd(complex: Complex): Complex

  /**
    * Method to multiply this by the given parameter (a Polar).
    *
    * @param multiplicand the complex multiplicand.
    * @return the product of this and multiplicand.
    */
  def product(multiplicand: Complex): Complex = this match {
    case ComplexPolar(_, _) => doMultiply(multiplicand)
    case ComplexCartesian(_, _) => multiplicand match {
      case ComplexCartesian(_, _) => doMultiply(multiplicand)
      case ComplexPolar(_, _) => narrow(this, polar = true) doMultiply multiplicand
    }
  }

  def doMultiply(complex: Complex): Complex

  protected def showImaginary: String = s"${if (imag.isPositive) "" else "-"}${imag.abs}"
}

object Complex {

  def unapply(arg: Complex): Option[(GeneralNumber, GeneralNumber)] = Some(arg.real, arg.imag)

  def convertToPolar(c: ComplexCartesian): Complex = {
    val ro: Option[Field] = for (p <- (c.x multiply c.x add c.y multiply c.y).asNumber; z = p.sqrt.materialize) yield z
    val z: Field = Field.recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    apply(z, c.y atan c.x, ComplexPolar.apply, ComplexException(s"logic error: convertToPolar2: $c"))
  }

  def convertToCartesian(c: ComplexPolar): Complex =
    apply(c.r multiply c.theta.cos, c.r multiply c.theta.sin, ComplexCartesian.apply, ComplexException(s"logic error: convertToCartesian: $c"))


  def apply(a: Field, b: Field, f: (GeneralNumber, GeneralNumber) => Complex, x: ComplexException): Complex =
    Field.recover(for (a <- a.asNumber; b <- b.asNumber) yield f(a, b), x)

  def narrow(x: Field, polar: Boolean): Complex = x match {
    case c@ComplexCartesian(_, _) => if (polar) convertToPolar(c) else c
    case c@ComplexPolar(_, _) => if (!polar) convertToCartesian(c) else c
    case n@GeneralNumber(_, _) => ComplexCartesian(n, GeneralNumber.zero)
  }
}

case class ComplexCartesian(x: GeneralNumber, y: GeneralNumber) extends Complex(x, y) {

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: GeneralNumber = magnitudeSquared.sqrt.asNumber.getOrElse(GeneralNumber.NaN)

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = for (f1 <- real.maybeFactor; f2 <- imag.maybeFactor; if f1 == f2) yield f1

  def numberProduct(n: GeneralNumber): Complex = make(x doMultiply n, y doMultiply n)

  def make(a: GeneralNumber, b: GeneralNumber): Complex = ComplexCartesian(a, b)

  def isZero: Boolean = x.isZero && y.isZero

  def isInfinite: Boolean = x.isInfinite || y.isInfinite

  def magnitudeSquared: Expression = (x doMultiply x) plus (y doMultiply y)

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = s"""($x $showImaginary"""

  /**
    * Add two Cartesian Complex numbers.
    *
    * @param complex the addend.
    * @return the sum.
    */
  def doAdd(complex: Complex): Complex = complex match {
    case ComplexCartesian(a, b) => Complex.apply((x plus a).materialize, (y plus b).materialize, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case c@ComplexPolar(_, _) => doAdd(convertToCartesian(c))
  }

  /**
    * Add two Cartesian Complex numbers.
    *
    * @param complex the addend.
    * @return the sum.
    */
  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexCartesian(a, b) =>
      val real: Expression = a * x plus -(b * y)
      val imag: Expression = a * y plus b * x
      Complex.apply(real.materialize, imag.materialize, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case ComplexPolar(_, _) => throw ComplexException("logic error: ComplexCartesian.doAdd")
  }
}

object ComplexCartesian {
  def apply(x: Int, y: Int): ComplexCartesian = ComplexCartesian(GeneralNumber(x), GeneralNumber(y))

  def apply(x: Int, y: GeneralNumber): ComplexCartesian = ComplexCartesian(GeneralNumber(x), y)

  def apply(x: GeneralNumber, y: Int): ComplexCartesian = ComplexCartesian(x, GeneralNumber(y))
}

case class ComplexPolar(r: GeneralNumber, theta: GeneralNumber) extends Complex(r, theta) {

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: GeneralNumber = r

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor of r) if factor of theta is Pi; otherwise None.
    */
  def maybeFactor: Option[Factor] = for (f1 <- real.maybeFactor; f2 <- imag.maybeFactor; if f2 == Pi) yield f1

  def numberProduct(n: GeneralNumber): Complex = make(r doMultiply n, theta)

  def make(a: GeneralNumber, b: GeneralNumber): Complex = ComplexPolar(a, b.modulate)

  def isZero: Boolean = r.isZero

  def isInfinite: Boolean = r.isInfinite

  def magnitudeSquared: Expression = r * r

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = s"${r}e^$showImaginary"

  def doAdd(complex: Complex): Complex = convertToCartesian(this).doAdd(complex)

  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexPolar(a, b) => make(r doMultiply a, theta doAdd b)
    case _ => throw ComplexException("logic error: ComplexPolar.doMultiply")
  }
}

object ComplexPolar {
  def apply(r: Int, theta: GeneralNumber): ComplexPolar = ComplexPolar(GeneralNumber(r), theta)
}

case class ComplexException(str: String) extends Exception(str)