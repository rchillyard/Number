package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.core.FuzzyNumber.withinWiggleRoom
import com.phasmidsoftware.number.core.Number.prepareWithSpecialize

import scala.util.Left

/**
  * This class is designed to model a fuzzy Number.
  * See Number for more details on the actual representation.
  *
  * TODO FuzzyNumber should be the "norm." ExactNumber is just a fuzzy number with None for fuzz.
  * I suspect that this inversion of logic might be the cause of some stack overflows that occur for some ignored tests.
  *
  * TODO ensure that every Double calculation contributes fuzziness.
  *
  * NOTE it is not necessary to override compareTo because the operative method is signum for all Number comparisons,
  * and that is overridden here.
  *
  * CONSIDER implementing equals (but be careful!). Don't implement it in terms of compare(...)==0 or Same.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  * @param fuzz   the fuzziness of this Number.
  */
case class FuzzyNumber(override val value: Value, override val factor: Factor, fuzz: Option[Fuzziness[Double]]) extends Number(value, factor) with Fuzz[Double] {

  /**
    * @return true if there is no fuzz.
    */
  def isExact: Boolean = fuzz.isEmpty

  /**
    * Auxiliary constructor for a FuzzyNumber.
    *
    * @param v    the value for the new Number.
    * @param fuzz the fuzz for the new Number.
    */
  def this(v: Value, fuzz: Option[Fuzziness[Double]]) = this(v, Scalar, fuzz)

  /**
    * Action to render this FuzzyNumber as a String.
    *
    * @return a String.
    */
  def render: String = toString

  /**
    * Action to materialize this Expression.
    *
    * @return this ExactNumber.
    */
  def materialize: Number = this

  /**
    * Add a Number to this FuzzyNumber.
    *
    * @param x the addend.
    * @return the sum.
    */
  override def doAdd(x: Number): FuzzyNumber = FuzzyNumber.plus(this, x)

  /**
    * Multiply a Number by this FuzzyNumber.
    *
    * @param x the multiplicand.
    * @return the product.
    */
  override def doMultiply(x: Number): FuzzyNumber = FuzzyNumber.times(this, x)

  override def makeNegative: FuzzyNumber = super.makeNegative.asFuzzyNumber

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
    * Raise this Number to the power p.
    * CONSIDER inlining this method.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  override def doPower(p: Number): Number = FuzzyNumber.power(this, p)

  /**
    * Method to compare this FuzzyNumber with another Number.
    *
    * @param other the other Number.
    * @return -1, 0, or 1 according to whether x is <, =, or > y.
    */
  override def compare(other: Number): Int = fuzzyCompare(other, 0.5)

  /**
    * @return true if this Number is equivalent to zero with at least 50% confidence.
    */
  override lazy val isZero: Boolean = isProbablyZero(0.5)

  /**
    * @param p the confidence desired. Ignored if isZero is true.
    * @return true if this Number is equivalent to zero with at least p confidence.
    */
  def isProbablyZero(p: Double): Boolean = super.isZero || (for (f <- fuzz; x <- toDouble) yield withinWiggleRoom(p, f, x)).getOrElse(false)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with better than evens confidence, then
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  override lazy val signum: Int = signum(0.5)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired.
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = if (isProbablyZero(p)) 0 else super.signum

  /**
    * Make a copy of this FuzzyNumber but with additional fuzz given by f.
    *
    * @param f the additional fuzz.
    * @return this but with fuzziness which is the convolution of fuzz and f.
    */
  def addFuzz(f: Fuzziness[Double]): Number = FuzzyNumber.addFuzz(this, f)

  /**
    * Make a copy of this Number, but with different fuzziness.
    *
    * @param z the optional Fuzz.
    * @return a FuzzyNumber.
    */
  def makeFuzzy(z: Option[Fuzziness[Double]]): FuzzyNumber = FuzzyNumber(value, factor, z)

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other        the other operand, a Number.
    * @param f            the factor to apply to the result.
    * @param op           the appropriate DyadicOperation.
    * @param independent  true if the fuzziness of the operands are independent.
    * @param coefficients an optional Tuple representing the coefficients to scale the fuzz values by.
    *                     For a power operation such as x to the power of y, these will be y/x and ln x respectively.
    *                     For addition or multiplication, they will be 1 and 1.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadicFuzzy(other: Number, f: Factor)(op: DyadicOperation, independent: Boolean, coefficients: Option[(Double, Double)]): Option[Number] =
    for (n <- composeDyadic(other, f)(op); t1 <- this.toDouble; t2 <- other.toDouble) yield
      FuzzyNumber(n.value, n.factor, Fuzziness.combine(t1, t2, !op.absolute, independent)(Fuzziness.applyCoefficients((fuzz, other.fuzz), coefficients)))

  /**
    * Evaluate a monadic operator on this, using either negate or... according to the value of op.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def transformMonadicFuzzy(f: Factor)(op: MonadicOperation): Option[Number] = {
    transformMonadic(f)(op).flatMap {
      case n: FuzzyNumber =>
        for (x <- n.toDouble) yield n.makeFuzzy(Fuzziness.map(x, !op.absolute, op.derivative, fuzz))
    }
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
      case None => Number.valueToString(value)
    }
    sb.append(w)
    sb.append(factor.toString)
    sb.toString
  }

  /**
    * NOTE: I'm not 100% sure if this is strictly necessary.
    *
    * @param that another object.
    * @return true if this can equal that.
    */
  override def canEqual(that: Any): Boolean = that match {
    case _: Number => true
    case x: AtomicExpression => x.materialize.asNumber.isDefined
    case _ => false
  }

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return a FuzzyNumber.
    */
  protected def make(v: Value, f: Factor): Number = make(v, f, fuzz)

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
  protected def make(v: Value, f: Factor, z: Option[Fuzziness[Double]]): Number = FuzzyNumber(v, f, z)

  /**
    * Ensure that this Number is actually a FuzzyNumber.
    *
    * @return a FuzzyNumber which is the same as this Number.
    */
  def asFuzzyNumber: FuzzyNumber = this
}

object FuzzyNumber {

  /**
    * Definition of concrete (implicit) type class object for FuzzyNumber being Fuzzy.
    */
  implicit object NumberIsFuzzy extends Fuzzy[Number] {
    /**
      * Method to determine if x1 and x2 can be considered the same with a probability of p.
      *
      * @param p  a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless x1 actually is x2.
      * @param x1 a value of X.
      * @param x2 a value of X.
      * @return true if x1 and x2 are considered equal with probability p.
      */
    def same(p: Double)(x1: Number, x2: Number): Boolean = x1.doAdd(x2.makeNegative).asFuzzyNumber.isProbablyZero(p)
  }

  def power(number: FuzzyNumber, p: Number): Number = composeDyadic(number, p, p.factor, DyadicOperationPower, independent = false, getPowerCoefficients(number, p))

  def apply(): Number = Number.apply()

  def sin(x: FuzzyNumber): Number = transformMonadic(x.scale(Pi), Scalar, MonadicOperationSin)

  // TEST me or eliminate
  def sqrt(x: FuzzyNumber): Number = transformMonadic(x, Scalar, MonadicOperationSqrt)

  /**
    * Get the fuzz coefficients for calculating the fuzz on a power operation.
    * According to the "Generalized Power Rule," these coefficients should be y/x and ln x, respectively,
    * where x is the magnitude of n and y is the magnitude of p.
    *
    * @param n the number to be raised to power p.
    * @param p the power (exponent) with which to raise n.
    * @return the value of n to the power of p.
    */
  private def getPowerCoefficients(n: Number, p: Number): Option[(Double, Double)] =
    for (z <- n.toDouble; q <- p.toDouble) yield (q / z, math.log(z))

  /**
    *
    * @param p the confidence desired. Ignored if isZero is true.
    * @param f the fuzziness.
    * @param x the value to be tested (may be positive or negative).
    * @return true if x is within the tolerance range of f, given confidence level p. Otherwise, false
    */
  private def withinWiggleRoom(p: Double, f: Fuzziness[Double], x: Double) = f.normalizeShape.wiggle(p) > math.abs(x)

  private def plus(x: FuzzyNumber, y: Number): FuzzyNumber = {
    val (p, q) = x.alignTypes(y)
    (p, q) match {
      case (n: FuzzyNumber, _) => composeDyadic(n, q, p.factor, DyadicOperationPlus, independent = true, None)
      case (_, n: FuzzyNumber) => composeDyadic(n, p, q.factor, DyadicOperationPlus, independent = true, None)
      case (_, _) => recover((p plus q).materialize.asNumber, FuzzyNumberException("logic error: plus")).asFuzzyNumber
    }
  }

  private def times(x: FuzzyNumber, y: Number): FuzzyNumber = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    (p, q) match {
      case (n: FuzzyNumber, _) => composeDyadic(n, q, p.factor, DyadicOperationTimes, independent = x != y, None)
      case (_, n: FuzzyNumber) => composeDyadic(n, p, q.factor, DyadicOperationTimes, independent = x != y, None)
      case (_, _) => recover((p multiply q).materialize.asNumber, FuzzyNumberException("logic error: times")).asFuzzyNumber
    }
  }

  private def addFuzz(n: Number, f: Fuzziness[Double]): Number = (n.value, n.fuzz) match {
    case (v@Left(Left(Some(_))), fo) => addFuzz(n, v, fo, f)
    case _ => n
  }

  private def addFuzz(number: Number, v: Value, fo: Option[Fuzziness[Double]], fAdditional: Fuzziness[Double]) = {
    val combinedFuzz = for (f <- fo.orElse(Some(AbsoluteFuzz(0.0, Box))); p <- number.toDouble; g <- Fuzziness.combine(p, 0, f.style, independent = false)((fo, fAdditional.normalize(p, f.style)))) yield g
    FuzzyNumber(v, number.factor, combinedFuzz)
  }

  /**
    * Evaluate a dyadic operator, defined by op, on n and q.
    * Parameter independent relates to the calculation of the error bounds of the result.
    *
    * @param n            the first operand.
    * @param q            the second operand.
    * @param f            the Factor to be used for the result.
    * @param op           the dyadic operation.
    * @param independent  true if the fuzziness of the inputs is independent..
    * @param coefficients an optional Tuple representing the coefficients to scale the fuzz values by.
    *                     For a power operation such as x to the power of y, these will be y/x and ln x respectively.
    *                     For addition or multiplication, they will be 1 and 1.
    * @return a new Number which is the result of operating on n and q as described above.
    */
  private def composeDyadic(n: Number, q: Number, f: Factor, op: DyadicOperation, independent: Boolean, coefficients: Option[(Double, Double)]): FuzzyNumber = n match {
    case x: FuzzyNumber => prepareWithSpecialize(x.composeDyadicFuzzy(q, f)(op, independent, coefficients)).asFuzzyNumber
    case _: Number => prepareWithSpecialize(n.composeDyadic(n, f)(op)).asFuzzyNumber
  }

  private def transformMonadic(n: Number, factor: Factor, op: MonadicOperation) = n match {
    case x: FuzzyNumber => prepareWithSpecialize(x.transformMonadicFuzzy(factor)(op))
    case _: Number => prepareWithSpecialize(n.transformMonadic(factor)(op))
  }

}

case class FuzzyNumberException(str: String) extends Exception(str)
