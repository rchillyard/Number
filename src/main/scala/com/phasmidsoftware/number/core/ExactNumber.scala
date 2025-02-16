/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * This class is designed to model an exact Numeral.
  * See GeneralNumber for more details on the actual representation.
  *
  * TODO implement scientific notation by having factors such os 10^3, 10^6, etc. (alternatively, add a separate parameter)
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Radian, and NatLog.
  */
case class ExactNumber(override val value: Value, override val factor: Factor) extends GeneralNumber(value, factor, None) {

  /**
    * Method to determine if this ExactNumber is equivalent to another Numerical (x).
    *
    * @param x the other numerical.
    * @return true if they are the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean = x match {
    case Real(n) => isSame(n)
    case n: FuzzyNumber => n.isSame(this)
    case n: ExactNumber => doSubtract(n).isZero
    case c: Complex => c.isSame(Real(this))
  }

  /**
    * @return true if this Number is equal to zero.
    */
  def isZero: Boolean = GeneralNumber.isZero(this)

  /**
   * @param context an optional Factor to be matched.
   * @return true if there is no fuzz AND if context is defined then it should match factor.
    */
  def isExactInContext(context: Context): Boolean = factorAsIs(context)

  /**
    * Method to make some trivial simplifications of this ExactNumber.
    *
    * @return either this Number or a simplified Number.
    */
  def simplify: Number = (factor, value) match {
    case (Logarithmic(_), Right(0)) => Number.one
    // XXX this handles all roots (of which there are currently only Root2 and Root3)
    case (Root(n), v) => v match {
      case Right(x) =>
        (Rational.squareRoots.get(x) map (make(_, Scalar))).getOrElse(this)
      case Left(Right(r)) => r.root(n) match {
        case Some(x) => ExactNumber(Value.fromRational(x), Scalar)
        case _ => this
      }
      case Left(Left(Some(_))) => scale(Scalar)
      case _ => this
    }
    case _ => this
  }

  /**
    * Add this Number to n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the sum of this and n.
    */
  def doAdd(n: Number): Number = this match {
    case x: GeneralNumber => GeneralNumber.plus(x, n)
  }

  /**
    * Multiply this Number by n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number = GeneralNumber.times(this, n)

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number = GeneralNumber.power(this, p)

  /**
    * Method to scale this ExactNumber by a constant factor.
    *
    * TESTME
    *
    * @param v the factor.
    * @return
    */
  def scale(v: Value): Number = GeneralNumber.doTimes(this, ExactNumber(v, Scalar), factor)

  /**
    * Method to compare this Number with another.
    *
    * @param other the other Number.
    * @return -1, 0, or 1 according to the relative magnitudes.
    */
  def compare(other: Number): Int = Number.doCompare(this, other)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * CONSIDER should be invoke simplify from other make methods?
    *
    * @param v the value.
    * @param f the factor.
    * @return an ExactNumber.
    */
  def make(v: Value, f: Factor): Number = ExactNumber(v, f)

  /**
    * We cannot add fuzziness to an Exact number so we return the equivalent FuzzyNumber.
    *
    * @param fo the (optional) fuzziness.
    * @return a Number.
    */
  def make(fo: Option[Fuzziness[Double]]): Number = FuzzyNumber(value, factor, fo)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * TESTME
    *
    * @param v  the value (a Double).
    * @param f  Factor.
    * @param fo optional fuzz.
    * @return either a Number.
    */
  def make(v: Double, f: Factor, fo: Option[Fuzziness[Double]]): Number = fo match {
    case None => ExactNumber(Value.fromDouble(Some(v)), f)
    case Some(_) => FuzzyNumber(Value.fromDouble(Some(v)), f, fo)
  }

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    * the result will be zero.
    *
    * TESTME
    *
    * @param p the confidence desired (ignored).
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * Method to determine if this Number is probably zero.
    * It is used particularly when comparing two Numbers to see if they are the same.
    *
    * @param p the confidence desired. Ignored.
   * @return true if this Number is actually zero.
    */
  def isProbablyZero(p: Double = 0.5): Boolean = isZero

  /**
   * Render this ExactNumber as a String representation.
   *
   * @return the String representation of this ExactNumber.
   */
  override def render: String = toString

  /**
    * Render this ExactNumber in String form, including the factor.
    *
    * @return a String
    */
  override def toString: String = modulate match {
    case Number.pi => Radian.toString
    case _ =>
      val sb = new StringBuilder()
      factor match {
        case Logarithmic(_) =>
          sb.append(factor.render(value))
        case PureNumber(_) =>
          sb.append(Value.valueToString(value))
          sb.append(factor.toString)
        case Root(_) =>
          sb.append(factor.render(value))
        case _ =>
          throw NumberException(s"factor is not matched: $factor")
      }
      sb.toString
  }
}

/**
 * Companion object for the `ExactNumber` class.
 *
 * Provides factory methods for creating instances of `ExactNumber`. These methods allow
 * creation with a given `Factor` or default to the `Scalar` factor. This object encapsulates
 * logic to initialize `ExactNumber` instances with consistent internal representations and initializes
 * the required values.
 */
object ExactNumber {

  /**
   * Creates a new instance of `ExactNumber` from a given integer value and a specified factor.
   *
   * @param x      the integer value to be encapsulated by the `ExactNumber`.
   * @param factor the factor associated with the `ExactNumber` instance.
   * @return a new `ExactNumber` instance with the specified value and factor.
   */
  def apply(x: Int, factor: Factor): ExactNumber = new ExactNumber(Value.fromInt(x), factor)

  /**
   * Creates an instance of `ExactNumber` with the value `x` and a default factor of `Scalar`.
   * TESTME
   *
   * @param x an integer representing the value of the `ExactNumber`.
   * @return an instance of `ExactNumber` with the specified value and the `Scalar` factor.
   */
  def apply(x: Int): ExactNumber = apply(x, Scalar)

//  def product(x: ExactNumber, y: ExactNumber): Number = (x, y) match {
//    case (ExactNumber(w, Scalar), b) => b.scale(w)
//    case (a, ExactNumber(w, Scalar)) => a.scale(w)
//    case (a, b) => // Neither a nor b has factor Scalar
//      val (p, q) = a.alignTypes(b)
//      (p.factor, q.factor) match {
//        case (f@PureNumber(_), Scalar) => doTimes(p, q, f)
//        case (Scalar, f@PureNumber(_)) => doTimes(p, q, f)
//        case (f: Logarithmic, Scalar) if q.signum > 0 => prepareWithSpecialize(p.composeDyadic(q.scale(f), f)(DyadicOperationPlus))
//        case (_: Logarithmic, Scalar) => times(p.scale(Scalar), q)
//        case (Root(_), Root(_)) if p == q => p.make(Scalar)
//        case (Root(_), Root(_)) => doTimes(p, q.scale(p.factor), p.factor)
//        case _ => times(p.scale(Scalar), q.scale(Scalar))
//      }
//  }
//
//  private def doTimes(p: Number, q: Number, factor: Factor) = prepareWithSpecialize(p.composeDyadic(q, factor)(DyadicOperationTimes))

}
