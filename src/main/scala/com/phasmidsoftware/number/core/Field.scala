package com.phasmidsoftware.number.core

/**
  * Trait which describes the behavior of all Numbers and Complex instances.
  * See https://en.wikipedia.org/wiki/Field_(mathematics).
  *
  * Currently, the only sub-classes of Field are Number and Complex.
  *
  * The operations supported are addition, subtraction, multiplication and division.
  * By inference, we should be able to raise an instance of Field to a numeric power.
  */
trait Field extends NumberLike {

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
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field

  /**
    * Method to return this Field as a Complex.
    *
    * @return either this or Complex(this) as appropriate.
    */
  def asComplex: Complex = this match {
    case n@Number(_, _) => Complex(n)
    case n@Complex(_, _) => n
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
  val sGamma = "0.57721566490153286060651209008240243104215933593992"
  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sBoltzmann = "1380649.E-29" // J K ^ -1

  val one: Number = Number.one
  val zero: Number = Number.zero
  val pi: Number = Number.pi
  val e: Number = Number.e
  val i: Complex = Complex.i

  val root2: Number = Number.root2
  val root3: Number = Number.root3
  val root5: Number = Number.root5

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
    * https://en.wikipedia.org/wiki/Gravitational_constant
    * We must use the string sG because of FIXME Issue #54
    */
  lazy val G: Number = Number(sG)

  /**
    * https://en.wikipedia.org/wiki/Fine-structure_constant
    */
  lazy val alpha: Number = 0.0072973525693 ~ 11 // (dimensionless)

  /**
    * https://en.wikipedia.org/wiki/Avogadro_constant
    */
  lazy val avagadro: Number = Number(6.0221407600E23)

  /**
    * https://en.wikipedia.org/wiki/Boltzmann_constant
    */
  lazy val boltzmann: Number = Number(sBoltzmann)

  /**
    * https://en.wikipedia.org/wiki/Planck_constant
    */
  lazy val planck: Number = Number("6.6260701500E-34") // J Hz ^ -1

  /**
    * https://en.wikipedia.org/wiki/Speed_of_light
    */
  lazy val c: Number = Number("299792458") // m sec ^ -1

  /**
    * https://en.wikipedia.org/wiki/Proton-to-electron_mass_ratio
    */
  lazy val mu: Number = 1836.15267343 ~ 11 // (dimensionless)
}
