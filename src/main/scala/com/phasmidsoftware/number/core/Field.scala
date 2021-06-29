package com.phasmidsoftware.number.core

trait Field extends AtomicExpression {
  def isInfinite: Boolean

  def isZero: Boolean

  def isExact: Boolean

  def asNumber: Option[Number] = this match {
    case n@Number(_, _) => Some(n)
    case ComplexCartesian(x, y) if y == Number.zero => Some(x)
    case ComplexPolar(r, theta) if theta == Number.zero => Some(r)
    case _ => None
  }


  /**
    * Add x to this Number and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field

  /**
    * Multiply this Number by x and return the result.
    * See Number.times for more detail.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
    * Divide this Number by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Field): Field

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field
}

object Field {
  def convertToNumber(field: Field): Number = recover(field.asNumber, NumberException(s"$field is not a Number"))

  def recover[T](to: Option[T], x: Throwable): T = to match {
    case Some(t) => t
    case None => throw x
  }
}