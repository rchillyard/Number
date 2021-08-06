package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar, narrow}
import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.core.Number.negate

abstract class Complex(val real: Number, val imag: Number) extends Field {

  /**
    * Evaluate the magnitude squared of this Complex number.
    *
    * @return the magnitude squared.
    */
  def magnitudeSquared: Number

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: Number

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
    * Change the sign of this Number.
    */
  def unary_- : Field = make(real, Number.negate(imag))

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
        for (r <- Literal(real).^(p).materialize.asNumber; i <- (Literal(imag) * p).materialize.asNumber) yield make(r, i),
        ComplexException("logic error: power")
      )
    case ComplexCartesian(_, _) => p.toInt match {
      case Some(0) => Complex.unit
      case Some(-1) => -this divide this.magnitudeSquared
      case Some(1) => this
      case Some(x) if x > 0 => LazyList.continually(this).take(x).toList.foldLeft[Complex](Complex.unit)((a, b) => a doMultiply b)
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

  protected def showImaginary: String = s"${if (imag.isPositive) "" else "-"}i${imag.abs}"
}

object Complex {
  val i: ComplexCartesian = ComplexCartesian(0, 1)
  val unit: ComplexCartesian = ComplexCartesian(1, 0)

  // TODO this is dangerous.
  def unapply(arg: Complex): Option[(Number, Number)] = Some(arg.real, arg.imag)

  def convertToPolar(c: ComplexCartesian): Complex = {
    val ro: Option[Field] = for (p <- ((Literal(c.x) * c.x) plus (Literal(c.y) * c.y)).materialize.asNumber; z = p.sqrt) yield z
    val z: Field = Field.recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    apply(z, c.x atan c.y, ComplexPolar.apply, ComplexException(s"logic error: convertToPolar2: $c"))
  }

  def convertToCartesian(c: ComplexPolar): Complex =
    apply(c.r multiply c.theta.cos, c.r multiply c.theta.sin, ComplexCartesian.apply, ComplexException(s"logic error: convertToCartesian: $c"))


  def apply(a: Field, b: Field, f: (Number, Number) => Complex, x: ComplexException): Complex =
    Field.recover(for (a <- a.asNumber; b <- b.asNumber) yield f(a, b), x)

  def apply(x: Number): Complex = ComplexCartesian(x, Number.zero)

  def narrow(x: Field, polar: Boolean): Complex = x match {
    case c@ComplexCartesian(_, _) => if (polar) convertToPolar(c) else c
    case c@ComplexPolar(_, _) => if (!polar) convertToCartesian(c) else c
    case n@Number(_, _) => ComplexCartesian(n, Number.zero)
  }
}

case class ComplexCartesian(x: Number, y: Number) extends Complex(x, y) {

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: Number = magnitudeSquared.sqrt

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = if (real.factor == imag.factor) Some(real.factor) else None

  def numberProduct(n: Number): Complex = make(x doMultiply n, y doMultiply n)

  def make(a: Number, b: Number): Complex = ComplexCartesian(a, b)

  def isZero: Boolean = convertToPolar(this).real.isProbablyZero(0.5)

  def isInfinite: Boolean = x.isInfinite || y.isInfinite

  def magnitudeSquared: Number = Literal(x) * x plus (Literal(y) * y)

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = s"""($x+$showImaginary)"""

  /**
    * Add two Cartesian Complex numbers.
    *
    * @param complex the addend.
    * @return the sum.
    */
  def doAdd(complex: Complex): Complex = complex match {
    case ComplexCartesian(a, b) =>
      import com.phasmidsoftware.number.core.Expression.ExpressionOps
      Complex.apply(Literal(x).+(a).materialize, (Literal(y) + b).materialize, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
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
      val real: Number = (a doMultiply x) doAdd negate(b doMultiply y)
      val imag: Number = (a doMultiply y) doAdd (b doMultiply x)
      Complex.apply(real, imag, ComplexCartesian.apply, ComplexException(s"logic error: ComplexCartesian.doAdd: $complex"))
    case ComplexPolar(_, _) => throw ComplexException("logic error: ComplexCartesian.doAdd")
  }
}

object ComplexCartesian {
  def apply(x: Int, y: Int): ComplexCartesian = ComplexCartesian(Number(x), Number(y))

  def apply(x: Int, y: Number): ComplexCartesian = ComplexCartesian(Number(x), y)

  def apply(x: Number, y: Int): ComplexCartesian = ComplexCartesian(x, Number(y))
}

case class ComplexPolar(r: Number, theta: Number) extends Complex(r, theta) {

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: Number = r

  /**
    * Method to determine if this Complex is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor of r) if factor of theta is Pi; otherwise None.
    */
  def maybeFactor: Option[Factor] = if (real.factor == imag.factor) Some(real.factor) else None

  def numberProduct(n: Number): Complex = make(r doMultiply n, theta)

  def make(a: Number, b: Number): Complex = ComplexPolar(a, b.modulate)

  def isZero: Boolean = r.isZero

  def isInfinite: Boolean = r.isInfinite

  def magnitudeSquared: Number = Literal(r) * r

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
  def apply(r: Int, theta: Number): ComplexPolar = ComplexPolar(Number(r), theta)
}

case class ComplexException(str: String) extends Exception(str)