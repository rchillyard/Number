package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.asComplex

abstract class Complex(val real: Number, val imag: Number) extends AtomicExpression with Field {
  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * NOTE: the implementations of this don't always make perfect sense regarding maybeFactor.
    *
    * @return true if materialize will result in an ExactNumber, else false.
    */
  def isExact: Boolean = real.isExact && imag.isExact

  /**
    * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
    *
    * TODO may need to revisit this as Polar complex numbers should be easily handled.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = for (f1 <- real.maybeFactor; f2 <- imag.maybeFactor; if f1 == f2) yield f1

  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = throw NumberException("cannot materialize a Complex")

  /**
    * Add x to this Number and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = addComplex(asComplex(x, polar = false))

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field = ???

  /**
    * Multiply this Number by x and return the result.
    * See Number.times for more detail.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = ???

  /**
    * Divide this Number by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = ???

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Number): Field = ???

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field = ???

  /**
    * Method to add this to the given parameter (a Cartesian).
    *
    * @param addend the complex addend.
    * @return the sum of this and addend.
    */
  def addComplex(addend: Complex): Complex = this match {
    case ComplexCartesian(_, _) => doAdd(addend)
    case ComplexPolar(_, _) => asComplex(this, polar = false) doAdd addend
  }

  def doAdd(complex: Complex): Complex

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

  def asComplex(x: Field, polar: Boolean): Complex = x match {
    case c@ComplexCartesian(_, _) => if (polar) convertToPolar(c) else c
    case n@Number(_, _) => ComplexCartesian(n, Number.zero)
  }

}

case class ComplexCartesian(x: Number, y: Number) extends Complex(x, y) {

  def isZero: Boolean = x.isZero && y.isZero

  def isInfinite: Boolean = x.isInfinite || y.isInfinite

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
}

case class ComplexPolar(r: Number, theta: Number) extends Complex(r, theta) {

  def isZero: Boolean = r.isZero

  def isInfinite: Boolean = r.isInfinite

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = s"${r}e^$showImaginary"

  def doAdd(complex: Complex): Complex = throw ComplexException("logic error: ComplexPolar.doAdd")

}

case class ComplexException(str: String) extends Exception(str)