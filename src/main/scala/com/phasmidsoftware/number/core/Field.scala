package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.recover
import scala.language.implicitConversions

/**
  * Trait which describes the behavior of all Numbers and Complex instances.
  * See [[https://en.wikipedia.org/wiki/Field_(mathematics)]].
  *
  * Currently, the only sub-classes of Field are Number and Complex.
  *
  * The operations supported are addition, subtraction, multiplication and division.
  * By inference, we should be able to raise an instance of Field to a numeric power.
  */
trait Field extends NumberLike with Ordered[Field] {

  /**
    * Method to determine if this Field has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean

  /**
    * Method to determine if this Field has zero magnitude.
    * Zero is the additive identity.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean

  /**
    * Method to determine if this Field has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean

  /**
    * Method to determine if this Field is represented by a Complex number.
    *
    * @return true if this is Complex.
    */
  def isComplex: Boolean = this match {
    case ComplexCartesian(_, _) | ComplexPolar(_, _, _) => true
    case _ => false
  }

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  def +(x: Field): Field = add(x)

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  def *(x: Field): Field = multiply(x)

  /**
    * Divide this Field by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field

  def /(x: Field): Field = divide(x)

  /**
    * Change the sign of this Field.
    */
  def unary_- : Field

  /**
    * Raise this Field to the power p.
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
    * Raise this Field to the power p.
    *
    * @param p a Field.
    * @return this Field raised to power p.
    */
  def power(p: Field): Field

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field

  /**
    * Method to return this Field as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex

  /**
    * Method to return this Field as a Real, if possible.
    * If this is a Real number x, return Some(x) otherwise, return None.
    *
    * @return an Option[Real].
    */
  def asReal: Option[Real]
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

  implicit def convertRationalToField(r: Rational): Field = Real(Number(r))

  /**
    * Definition of concrete (implicit) type class object for Field being Fuzzy.
    */
  implicit object FieldIsFuzzy extends Fuzzy[Field] {
    /**
      * Method to determine if x1 and x2 can be considered the same with a probability of p.
      *
      * @param p  a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless x1 actually is x2.
      * @param x1 a value of X.
      * @param x2 a value of X.
      * @return true if x1 and x2 are considered equal with probability p.
      */
    def same(p: Double)(x1: Field, x2: Field): Boolean = x1.add(-x2).asNumber.exists(_.isProbablyZero(p))
  }

}

object Constants {
  val sPhi = "1.618033988749894"
  val sGamma = "0.57721566490153286060651209008240243104215933593992*"
  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sBoltzmann = "1380649.E-29" // J K ^ -1

  val one: Number = Number.one
  val zero: Number = Number.zero
  val pi: Number = Number.pi
  //noinspection NonAsciiCharacters
  val `𝛑`: Number = Number.`𝛑`
  val e: Number = Number.e
  val i: Complex = Complex.i
  /**
    * Exact value of iPi.
    */
  val iPi: Complex = ComplexCartesian(0, Number.pi)

  val root2: Number = Number.root2
  val root3: Number = Number.root3
  val root5: Number = Number.root5
  /**
    * Exact value of the Complex Number √2
    */
  val root2s: Field = ComplexPolar(Number.root2, Number.zeroR, 2)

  import com.phasmidsoftware.number.core.Number.FuzzOps

  /**
    * https://en.wikipedia.org/wiki/Golden_ratio
    */
  lazy val phi: Number = Number(sPhi)

  /**
    * https://en.wikipedia.org/wiki/Euler–Mascheroni_constant
    */
  lazy val gamma: Number = Number(sGamma)

  /**
    * https://en.wikipedia.org/wiki/Gravitational_constant (exact).
    * We must use the string sG because of FIXME Issue #54
    */
  lazy val G: Number = Number(sG)

  /**
    * https://en.wikipedia.org/wiki/Fine-structure_constant
    */
  lazy val alpha: Number = 0.0072973525693 ~ 11 // (dimensionless)

  /**
    * https://en.wikipedia.org/wiki/Avogadro_constant (exact).
    */
  lazy val avagadro: Number = Number(6.0221407600E23)

  /**
    * https://en.wikipedia.org/wiki/Boltzmann_constant (exact).
    */
  lazy val boltzmann: Number = Number(sBoltzmann)

  /**
    * https://en.wikipedia.org/wiki/Planck_constant (exact).
    */
  lazy val planck: Number = Number("6.6260701500E-34") // J Hz ^ -1

  /**
    * https://en.wikipedia.org/wiki/Speed_of_light (exact).
    */
  lazy val c: Number = Number("299792458") // m sec ^ -1

  /**
    * https://en.wikipedia.org/wiki/Proton-to-electron_mass_ratio
    */
  lazy val mu: Number = 1836.15267343 ~ 11 // (dimensionless)
}
