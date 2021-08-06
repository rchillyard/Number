package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Field.recover

/**
  * Trait which describes the behavior of all Numbers and Complex instances.
  * See https://en.wikipedia.org/wiki/Field_(mathematics).
  *
  * Currently, the only sub-classes of Field are Number and Complex.
  *
  * The operations supported are addition, subtraction, multiplication and division.
  * By inference, we should be able to raise an instance of Field to a numeric power.
  */
trait Field {

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
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
    * Divide this Field by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field

  /**
    * Change the sign of this Field.
    */
  def unary_- : Field

  /**
    * Raise this Field to the power p.
    *
    * @param p a Number.
    * @return this Field raised to power p.
    */
  def power(p: Number): Field

  /**
    * Raise this Field to the power p.
    *
    * @param p an Int.
    * @return this Field raised to power p.
    */
  def power(p: Int): Field

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field

  /**
    * Method to determine if this Field is actually a real Number (i.e. not complex).
    * NOTE: to force this as a Number, use convertToNumber in the companion Object.
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number] =
    this match {
      case n@Number(_, _) => Some(n)
      case ComplexCartesian(x, y) if y == Number.zero => Some(x)
      case ComplexPolar(r, theta) if theta == Number.zero => Some(r)
      case ComplexPolar(r, theta) if theta == Number.pi => Some(r.makeNegative)
      case _ => None
    }

  /**
    * Method to return this Field as a Complex.
    *
    * @return either this or Complex(this) as appropriate.
    */
  def asComplex: Complex = this match {
    case n@Number(_, _) => Complex(n)
    case n@Complex(_, _) => n
  }

  /**
    * Method to render this Field in a presentable manner.
    *
    * @return a String
    */
  def render: String

  /**
    * Eagerly compare this Field with comparand.
    *
    * TODO this will work only for Numbers. We need to be able to determine if two Complex numbers are essentially the same.
    *
    * @param comparand the expression to be compared.
    * @return the result of comparing this with comparand, as Numbers.
    *         An exception is thrown if either cannot be represented as a Number.
    */
  def compare(comparand: Field): Int = (this, comparand) match {
    case (x@Number(_, _), y@Number(_, _)) => x.compare(y)
    case (fx, fy) => recover(for (x <- fx.asNumber; y <- fy.asNumber) yield x.compare(y), NumberException("cannot compare Complex numbers"))
  }
}

object Field {
  /**
    * Attempt to force the given field to be a Number.
    * Because this may throw an Exception, it is much better to use asNumber, an instance method of Field.
    *
    * @param field the given field.
    * @return a Number if field is a Number, otherwise, this will throw a NumberException.
    */
  def convertToNumber(field: Field): Number = recover(field.asNumber, NumberException(s"$field is not a Number"))

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

object Constants {
  val sPhi = "1.618033988749894"
  val sGamma = "0.57721566490153286060651209008240243104215933593992"
  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sAlpha = "0.0072973525693(11)" // (dimensionless)
  val sAvagadro = "6.0221407600E23" // mole ^ -1
  val sBoltzmann = "1380649.E-29" // J K ^ -1
  val sPlanck = "6.6260701500E-34" // J Hz ^ -1
  val sC = "299792458" // m sec ^ -1
  val sMu = "1836.15267343(11)" // (dimensionless)

  val one: Number = Number.one
  val zero: Number = Number.zero
  val pi: Number = Number.pi
  val e: Number = Number.e
  val i: Complex = Complex.i

  /**
    * https://en.wikipedia.org/wiki/Golden_ratio
    */
  lazy val phi: Number = Number(sPhi)

  /**
    * https://en.wikipedia.org/wiki/Euler–Mascheroni_constant
    */
  lazy val gamma: Number = Number(sGamma)

  /**
    * https://en.wikipedia.org/wiki/Gravitational_constant
    */
  lazy val G: Number = Number(sG)

  /**
    * https://en.wikipedia.org/wiki/Fine-structure_constant
    */
  lazy val alpha: Number = Number(sAlpha)

  /**
    * https://en.wikipedia.org/wiki/Avogadro_constant
    */
  lazy val avagadro: Number = Number(sAvagadro)

  /**
    * https://en.wikipedia.org/wiki/Boltzmann_constant
    */
  lazy val boltzmann: Number = Number(sBoltzmann)

  /**
    * https://en.wikipedia.org/wiki/Planck_constant
    */
  lazy val planck: Number = Number(sPlanck)

  /**
    * https://en.wikipedia.org/wiki/Speed_of_light
    */
  lazy val c: Number = Number(sC)

  /**
    * https://en.wikipedia.org/wiki/Proton-to-electron_mass_ratio
    */
  lazy val mu: Number = Number(sMu)
}
