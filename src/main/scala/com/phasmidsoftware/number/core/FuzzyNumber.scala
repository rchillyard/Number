package com.phasmidsoftware.number.core

/**
  * This class is designed to model a fuzzy Number.
  * See Number for more details on the actual representation.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  * @param fuzz   the fuzziness of this Number.
  */
case class FuzzyNumber(override val value: Value, override val factor: Factor, fuzz: Option[Fuzz[Double]]) extends Number(value, factor) with Fuzzy[Double] {

  /**
    * Auxiliary constructor for an exact number.
    *
    * @param v    the value for the new Number.
    * @param fuzz the fuzz for the new Number.
    */
  def this(v: Value, fuzz: Option[Fuzz[Double]]) = this(v, Scalar, fuzz)

  /**
    * Add a Number to this FuzzyNumber.
    *
    * @param x the addend.
    * @return the sum.
    */
  override def +(x: Number): Number = FuzzyNumber.plus(this, x)

  /**
    * Multiply a Number by this FuzzyNumber.
    *
    * @param x the multiplicand.
    * @return the product.
    */
  override def *(x: Number): Number = FuzzyNumber.times(this, x)

  /**
    * Yields the square root of this FuzzyNumber.
    * If possible, the result will be exact.
    */
  override def sqrt: Number = FuzzyNumber.sqrt(this)

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    */
  override def sin: Number = FuzzyNumber.sin(this)

  /**
    * Change the sign of this Number.
    *
    * NOTE: don't think we really need this, do we?
    */
  override lazy val unary_- : Number = FuzzyNumber.negate(this)

  /**
    * @return true if this Number is equivalent to zero.
    */
  override lazy val isZero: Boolean = super.isZero || (for (f <- fuzz; x <- toDouble) yield f.normalizeShape.likely > math.abs(x)).getOrElse(false)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero, then
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  override lazy val signum: Int = if (isZero) 0 else super.signum

  /**
    * This method is invoked by power so do NOT invoke sqrt or power in implementations.
    *
    * NOTE: we do not add any extra degree of imprecision here because it's assumed that
    * any existing fuzziness will outweigh the double-precision-induced fuzziness.
    *
    * @return a Number which is the square toot of this, possibly fuzzy, Number.
    */
  def makeFuzzyIfAppropriate(f: Number => Number): Number = f(this)

  /**
    * Make a copy of this Number, but with different fuzziness.
    *
    * @param z the optional Fuzz.
    * @return either a Fuzzy or Exact Number.
    */
  def makeNumberFuzzy(z: Option[Fuzz[Double]]): FuzzyNumber = FuzzyNumber(value, factor, z)

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other    the other operand, a Number.
    * @param f        the factor to apply to the result.
    * @param op       the appropriate DyadicOperation.
    * @param absolute true if the convolution of Fuzz values should be absolute (addition) vs. relative (multiplication).
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadicFuzzy(other: Number, f: Factor)(op: DyadicOperation, absolute: Boolean): Option[Number] = {
    val no = composeDyadic(other, f)(op)
    no.map(n => FuzzyNumber(n.value, n.factor, Fuzz.combine(n.toDouble.get, !absolute, fuzz, other.fuzz))) // FIXME don't use get
  }

  /**
    * Evaluate a monadic operator on this, using either negate or... according to the value of op.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def composeMonadicFuzzy(f: Factor)(op: MonadicOperation, fuzzOp: Double => Double, absolute: Boolean): Option[Number] = {
    val no = composeMonadic(f)(op)
    no.map { case n: FuzzyNumber => n.makeNumberFuzzy(Fuzz.map(n.toDouble.get, !absolute, fuzzOp, fuzz)) } // FIXME don't use get
  }

  /**
    * Render this FuzzyNumber in String form, including the factor, and the fuzz.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    val w = fuzz match {
      case Some(f) => f.toString(toDouble.getOrElse(0.0))
      case None => valueToString
    }
    sb.append(w)
    sb.append(factor.toString)
    sb.toString
  }

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return a FuzzyNumber.
    */
  protected def makeNumber(v: Value, f: Factor): Number = makeNumber(v, f, fuzz)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    * CONSIDER: not entirely sure we need this method.
    *
    * @param v the value.
    * @param f the factor.
    * @param z the new fuzziness.
    * @return a FuzzyNumber.
    */
  protected def makeNumber(v: Value, f: Factor, z: Option[Fuzz[Double]]): Number = FuzzyNumber(v, f, z)
}

object FuzzyNumber {
  def apply(): Number = Number.apply()

  def sin(x: FuzzyNumber): Number = composeMonadic(x, MonadicOperationSin, x => math.cos(x), absolute = false)

  def sqrt(x: FuzzyNumber): Number = composeMonadic(x, MonadicOperationSqrt, x => x / 2, absolute = false)

  def negate(x: FuzzyNumber): Number = composeMonadic(x, MonadicOperationNegate, identity, absolute = false)

  def scale(x: FuzzyNumber, c: Int): Number = composeMonadic(x, MonadicOperationScale(c), identity, absolute = false)

  private def plus(x: FuzzyNumber, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    (p, q) match {
      case (n: FuzzyNumber, _) => composeDyadic(n, p, q, DyadicOperationPlus, absolute = true)
      case (_, n: FuzzyNumber) => composeDyadic(n, q, p, DyadicOperationPlus, absolute = true)
      case (_, _) => p + q
    }
  }

  private def times(x: FuzzyNumber, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    (p, q) match {
      case (n: FuzzyNumber, _) => composeDyadic(n, p, q, DyadicOperationTimes, absolute = false)
      case (_, n: FuzzyNumber) => composeDyadic(n, q, p, DyadicOperationTimes, absolute = false)
      case (_, _) => p * q
    }
  }

  private def composeDyadic(n: FuzzyNumber, p: Number, q: Number, op: DyadicOperation, absolute: Boolean) =
    n.composeDyadicFuzzy(q, p.factor)(op, absolute).getOrElse(Number()).specialize

  private def composeMonadic(n: FuzzyNumber, op: MonadicOperation, fuzzOp: Double => Double, absolute: Boolean) =
    n.composeMonadicFuzzy(n.factor)(op, fuzzOp, absolute).getOrElse(Number()).specialize
}

case class FuzzyNumberException(str: String) extends Exception(str)
