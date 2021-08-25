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
    * Method to make some trivial simplifications of this ExactNumber.
    *
    * @return either this Number or a simplified Number.
    */
  def simplify: Number = (factor, value) match {
    case (Logarithmic(_), Right(0)) => Number.one
    case (Root(2), v) => v match {
      case Right(x) =>
        (Rational.squareRoots.get(x) map (make(_, Scalar))).getOrElse(this)
      case Left(Right(r)) =>
        (for (n <- Rational.squareRoots.get(r.n.toInt); d <- Rational.squareRoots.get(r.d.toInt)) yield make(Rational(n, d))).getOrElse(this)
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
  def doPower(p: Number): Number = Number.power(this, p)

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
    * TEST me
    *
    * @param p the confidence desired (ignored).
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * @param p the confidence desired. Ignored if isZero is true.
    * @return true if this Number is equivalent to zero with at least p confidence.
    */
  def isProbablyZero(p: Double): Boolean = isZero

  /**
    * Render this ExactNumber in String form, including the factor.
    *
    * @return a String
    */
  override def toString: String = {
    val sb = new StringBuilder()
    factor match {
      case Logarithmic(_) =>
        sb.append(factor.render(value))
      case PureNumber(_) =>
        sb.append(Value.valueToString(value))
        sb.append(factor.toString)
      case Root(_) =>
        sb.append(factor.render(value))
    }
    sb.toString
  }

}
