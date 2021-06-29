package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.narrow
import com.phasmidsoftware.number.core.Field.{convertToNumber, recover}
import com.phasmidsoftware.number.core.Number.negate

abstract class Complex(val real: Number, val imag: Number) extends AtomicExpression with Field {
  /**
    * Instance method to make a Complex number from a real and an imaginary part.
    *
    * @param a the real part.
    * @param b the imaginary part.
    * @return a Complex number, either ComplexCartesian or ComplexPolar
    */
  def make(a: Number, b: Number): Complex

  /**
    * Method to determine if this Complex can be evaluated exactly.
    *
    * NOTE: the implementations of this don't always make perfect sense regarding maybeFactor.
    *
    * @return true if materialize will result in an exact Field, else false.
    */
  def isExact: Boolean = real.isExact && imag.isExact

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * TODO may need to revisit this as Polar complex numbers should be easily handled.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = for (f1 <- real.maybeFactor; f2 <- imag.maybeFactor; if f1 == f2) yield f1

  /**
    * Action to materialize this Complex as a Field,
    *
    * @return this.
    */
  def materialize: Field = this

  /**
    * Evaluate the magnitude squared of this Complex number.
    *
    * @return the magnitude squared.
    */
  def magnitudeSquared: Number

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
  def unary_- : Field = make(real, negate(imag))

  def numberProduct(n: Number): Complex

  /**
    * Multiply this Complex by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case n@Number(_, _) => numberProduct(n)
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
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Number): Field = this match {
    case ComplexPolar(_, _) =>
      recover(
        for (r <- real.^(p).materialize.asNumber; i <- (imag * p).materialize.asNumber) yield make(r, i),
        ComplexException("logic error: power")
      )
    case ComplexCartesian(a, b) => p.toInt match {
      case Some(0) => ComplexCartesian(Number.one, Number.zero)
      case Some(-1) => -this divide this.magnitudeSquared
      case Some(x) if x > 0 => LazyList.continually(this).take(x).toList.reduce[Expression]((a, b) => a * b).materialize
      case _ => throw ComplexException(s"not implemented: power($p)")
    }
  }

  def power(p: Int): Field = power(Number(p))

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

  def unapply(arg: Complex): Option[(Number, Number)] = Some(arg.real, arg.imag)

  def convertToPolar(c: ComplexCartesian): Complex = {
    val ro: Option[Field] = for (p <- (c.x multiply c.x add c.y multiply c.y).asNumber; z = p.sqrt.materialize) yield z
    val z: Field = Field.recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    apply(z, c.y atan c.x, ComplexPolar, ComplexException(s"logic error: convertToPolar2: $c"))
  }

  //  , ComplexPolar, ComplexException(s"logic error: convertToPolar: $c"))
  //  {
  //    val ro: Option[Number] = for (p <- (c.x multiply c.x add c.y multiply  c.y).asNumber; z <- p.sqrt.materialize.asNumber) yield z
  //    val to: Option[Number] = for (z <- (c.y atan c.x).asNumber) yield z
  //    val zo = for (r <- ro; t <- to) yield ComplexPolar(r, t)
  //    recover(zo, ComplexException(s"logic error: convertToPolar: $c"))
  //  }

  def convertToCartesian(c: ComplexPolar): Complex =
    apply(c.r multiply c.theta.cos, c.r multiply c.theta.sin, ComplexCartesian, ComplexException(s"logic error: convertToCartesian: $c"))


  def apply(a: Field, b: Field, f: (Number, Number) => Complex, x: ComplexException): Complex =
    Field.recover(for (a <- a.asNumber; b <- b.asNumber) yield f(a, b), x)

  def narrow(x: Field, polar: Boolean): Complex = x match {
    case c@ComplexCartesian(_, _) => if (polar) convertToPolar(c) else c
    case n@Number(_, _) => ComplexCartesian(n, Number.zero)
  }

}

case class ComplexCartesian(x: Number, y: Number) extends Complex(x, y) {

  def numberProduct(n: Number): Complex = make(x doMultiply n, y doMultiply n)

  def make(a: Number, b: Number): Complex = ComplexCartesian(a, b)

  def isZero: Boolean = x.isZero && y.isZero

  def isInfinite: Boolean = x.isInfinite || y.isInfinite

  def magnitudeSquared: Number = convertToNumber(((x doMultiply x) plus (y doMultiply y)).materialize)

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
    case ComplexCartesian(a, b) => Complex.apply((x plus a).materialize, (y plus b).materialize, ComplexCartesian, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case ComplexPolar(_, _) => throw ComplexException("logic error: ComplexCartesian.doAdd")
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
      Complex.apply(real.materialize, imag.materialize, ComplexCartesian, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case ComplexPolar(_, _) => throw ComplexException("logic error: ComplexCartesian.doAdd")
  }
}

case class ComplexPolar(r: Number, theta: Number) extends Complex(r, theta) {

  def numberProduct(n: Number): Complex = make(r doMultiply n, theta)

  def make(a: Number, b: Number): Complex = ComplexPolar(a, b)

  def isZero: Boolean = r.isZero

  def isInfinite: Boolean = r.isInfinite

  def magnitudeSquared: Number = r doMultiply r

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = s"${r}e^$showImaginary"

  def doAdd(complex: Complex): Complex = throw ComplexException("logic error: ComplexPolar.doAdd")

  def doMultiply(complex: Complex): Complex = complex match {
    case ComplexPolar(a, b) => make(r doMultiply a, theta doAdd b)
    case _ => throw ComplexException("logic error: ComplexPolar.doMultiply")
  }
}

case class ComplexException(str: String) extends Exception(str)