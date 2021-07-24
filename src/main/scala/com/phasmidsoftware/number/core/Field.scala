package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Field.recover

/**
  * Trait which describes the behavior of all Numbers and Complex instances.
  * See https://en.wikipedia.org/wiki/Field_(mathematics).
  *
  * Currently, the only sub-classes of Field are GeneralNumber and Complex.
  *
  * The operations supported are addition, subtraction, multiplication and division.
  * By inference, we should be able to raise an instance of Field to a numeric power.
  */
trait Field extends AtomicExpression {
  /**
    * Method to determine if this Field has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean

  /**
    * Method to determine if this Field has zero magnitude.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean

  /**
    * Method to determine if this Field is exact.
    *
    * @return true if this is exact, else false if this is fuzzy.
    */
  def isExact: Boolean

  /**
    * Add x to this Field and return the result.
    * See GeneralNumber.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  /**
    * Change the sign of this GeneralNumber.
    */
  def unary_- : Field

  /**
    * Multiply this Field by x and return the result.
    * See GeneralNumber.times for more detail.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
    * Divide this Field by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field

  /**
    * Raise this Field to the power p.
    *
    * @param p a GeneralNumber.
    * @return this Field raised to power p.
    */
  def power(p: GeneralNumber): Field

  /**
    * Raise this Field to the power p.
    *
    * @param p an Int.
    * @return this Field raised to power p.
    */
  def power(p: Int): Field

  /**
    * Yields the inverse of this Field.
    * This GeneralNumber is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field

  /**
    * Method to determine if this Field is actually a real GeneralNumber (i.e. not complex).
    * NOTE: to force this as a GeneralNumber, use convertToNumber in the companion Object.
    *
    * @return a Some(x) if this is a GeneralNumber; otherwise return None.
    */
  override def asNumber: Option[GeneralNumber] = this match {
    case n@GeneralNumber(_, _) => Some(n)
    case ComplexCartesian(x, y) if y == GeneralNumber.zero => Some(x)
    case ComplexPolar(r, theta) if theta == GeneralNumber.zero => Some(r)
    case ComplexPolar(r, theta) if theta == GeneralNumber.pi => Some(r.makeNegative)
    case _ => None
  }

  /**
    * Evaluate the magnitude squared of this Complex number.
    *
    * @return the magnitude squared.
    */
  def magnitudeSquared: Expression

  /**
    * Eagerly compare this Field with comparand.
    *
    * TODO this will work only for Numbers. We need to be able to determine if two Complex numbers are essentially the same.
    *
    * @param comparand the expression to be compared.
    * @return the result of comparing this with comparand, as Numbers.
    *         An exception is thrown if either cannot be represented as a GeneralNumber.
    */
  def compare(comparand: Field): Int = (this, comparand) match {
    case (x@GeneralNumber(_, _), y@GeneralNumber(_, _)) => x.compare(y)
    case (fx, fy) => recover(for (x <- fx.asNumber; y <- fy.asNumber) yield x.compare(y), NumberException("cannot compare Complex numbers"))
  }
}

object Field {
  /**
    * Attempt to force the given field to be a GeneralNumber.
    * Because this may throw an Exception, it is much better to use asNumber, an instance method of Field.
    *
    * @param field the given field.
    * @return a GeneralNumber if field is a GeneralNumber, otherwise, this will throw a NumberException.
    */
  def convertToNumber(field: Field): GeneralNumber = recover(field.asNumber, NumberException(s"$field is not a GeneralNumber"))

  /**
    * TODO: move this to a utility class.
    *
    * @param to an Option[T].
    * @param x  a Throwable to be thrown if to is None.
    * @tparam T the underlying type of to.
    * @return t if to is Some(t); otherwise x will be thrown.
    */
  def recover[T](to: Option[T], x: Throwable): T = to match {
    case Some(t) => t
    case None => throw x
  }
}