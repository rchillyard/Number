/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FuzzyNumber.{Ellipsis, withinWiggleRoom}
import com.phasmidsoftware.number.core.Number.prepareWithSpecialize

import scala.collection.mutable

/**
 * This class is designed to model a fuzzy Number.
 * See GeneralNumber for more details on the actual representation.
 *
 * CONSIDER FuzzyNumber should be the "norm." ExactNumber is just a fuzzy number with None for fuzz.
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
 * @param factor the scale factor of the Number: valid scales are: PureNumber, Radian, and NatLog.
 * @param fuzz   the fuzziness of this Number.
 */
case class FuzzyNumber(override val nominalValue: Value, override val factor: Factor, override val fuzz: Option[Fuzziness[Double]]) extends GeneralNumber(nominalValue, factor, fuzz) with Fuzz[Double] {

  /**
   * Method to determine if this FuzzyNumber is equivalent to another Numerical (x).
   *
   * @param x the other numerical.
   * @return true if they are the same, otherwise false.
   */
  def isSame(x: Numerical): Boolean = x match {
    case Real(n) => isSame(n)
    case n: Number => fuzzyCompare(n, 0.5) == 0
    case c: Complex => c.isSame(Real(this))
  }

  /**
   * Method to force the fuzziness of this FuzzyNumber to be absolute.
   * CONSIDER why do we make the fuzziness absolute here?
   *
   * @return the same FuzzyNumber but with absolute fuzziness.
   */
  def normalizeFuzz: Number = this match {
    case FuzzyNumber(value, factor, maybeFuzz) =>
      val relativeFuzz = for {
        x <- toDouble
        fuzz <- maybeFuzz
        normalized <- fuzz.normalize(x, relative = false)
      } yield normalized
      FuzzyNumber(value, factor, relativeFuzz)
    case x => x
  }

  /**
   *
   * @return either this Number or a simplified Number.
   */
  def simplify: Number = fuzz match {
    case None => ExactNumber(nominalValue, factor).simplify
    case _ =>
      factor match {
        case Root(_) => scale(PureNumber)
        case _ => this
      }
  }

  /**
   * @param context an optional Factor to be matched.
   * @return true if there is no fuzz AND if context is defined then it should match factor.
   */
  def isExactInContext(context: Context): Boolean = fuzz.isEmpty && factorAsIs(context)

  /**
   * Add a Number to this FuzzyNumber.
   *
   * @param x the addend.
   * @return the sum.
   */
  def doAdd(x: Number): Number = FuzzyNumber.plus(this, x) // NOTE: required but why?

  /**
   * Multiply a Number by this FuzzyNumber.
   *
   * @param x the multiplicand.
   * @return the product.
   */
  def doMultiply(x: Number): Number = FuzzyNumber.times(this, x) // NOTE: required but why?

  /**
   * Raise this Number to the power p.
   * NOTE this method can be eliminated but the unit tests will be a little off. Need to understand exactly why.
   *
   * @param p a Number.
   * @return this Number raised to power p.
   */
  def doPower(p: Number): Number = FuzzyNumber.power(this, p)

  /**
   * Method to compare this FuzzyNumber with another Number.
   * TESTME (partial)
   *
   * NOTE it appears that this method can be eliminated but the unit tests would not work without it.
   * Presumably, this is because the implicit equality checks in ScalaTest invoke compare. (?)
   *
   * @param other the other Number.
   * @return -1, 0, or 1 according to whether x is <, =, or > y.
   */
  def compare(other: Number): Int = fuzzyCompare(other, 0.5)

  /**
   * NOTE this method can be eliminated but the compare query operation doesn't appear to take fuzziness into account.
   *
   * @return true if this Number is equivalent to zero with at least 50% confidence.
   */
  def isZero: Boolean = isProbablyZero()

  /**
   * Method to determine the sense of this number: negative, zero, or positive.
   * If this FuzzyNumber cannot be distinguished from zero with better than evens confidence, then
   *
   * NOTE this method can be eliminated but the unit tests will be off. Need to understand exactly why.
   *
   * @return an Int which is negative, zero, or positive according to the magnitude of this.
   */
  override def signum: Int = signum(0.5)

  /**
   * Method to determine the sense of this number: negative, zero, or positive.
   * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
   *
   * @param p the confidence desired.
   * @return an Int which is negative, zero, or positive according to the magnitude of this.
   */
  def signum(p: Double): Int = if (isProbablyZero(p)) 0 else super.signum

  /**
   * @param p the confidence desired. Ignored if isZero is true.
   * @return true if this Number is equivalent to zero with at least p confidence.
   */
  def isProbablyZero(p: Double = 0.5): Boolean = GeneralNumber.isZero(this) || (for (f <- fuzz; x <- toDouble) yield withinWiggleRoom(p, f, x)).getOrElse(false)

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
    for (n <- composeDyadic(other, f)(op); t1 <- this.toDouble; t2 <- other.toDouble) yield {
      val q = Fuzziness.combine(t1, t2, !op.absolute, independent)(Fuzziness.applyCoefficients((fuzz, other.fuzz), coefficients))
      FuzzyNumber(n.nominalValue, n.factor, q)
    }

  /**
   * Render this FuzzyNumber as a String representation.
   *
   * @return a String representation of this FuzzyNumber.
   */
  override def render: String = toString

  /**
   * Render this FuzzyNumber in String form, including the factor, and the fuzz.
   *
   * TODO fuzzy zero (use createFuzzy(0)) renders as "0*" which I think is incorrect.
   *
   * @return
   */
  override def toString: String = {
    val sb = new mutable.StringBuilder()
    lazy val valueAsString = Value.valueToString(nominalValue, fuzz.isEmpty)
    val z = fuzz match {
      // CONSIDER will the following test work in all cases?
      case Some(f) if f.wiggle(0.5) > 1E-16 => f.toString(toDouble.getOrElse(0.0))
      case Some(_) => true -> (valueAsString.replace(Ellipsis, "") + "*")
      case None => true -> valueAsString
    }
    val w = z match {
      case (true, s) => s
      case (false, s) => valueAsString + "\u00B1" + s
    }
    factor match {
      case Logarithmic(_) =>
        sb.append(factor.render(w))
      case Scalar(_) =>
        sb.append(w)
        sb.append(factor.toString)
      case Root(_) =>
        sb.append(factor.render(w))
      case _ =>
        throw NumberException(s"factor is not matched: $factor")
    }
    sb.toString
  }


  /**
   * Make a copy of this Number, given the same degree of fuzziness as the original.
   * Both the nominalValue and the factor will be changed.
   *
   * @param v the nominalValue.
   * @param f the factor.
   * @return a FuzzyNumber.
   */
  def make(v: Value, f: Factor): Number = FuzzyNumber(v, f, fuzz)

  /**
   * Make a copy of this Number, with the same nominalValue and factor but with a different nominalValue of fuzziness.
   *
   * @param fo the (optional) fuzziness.
   * @return a Number.
   */
  def make(fo: Option[Fuzziness[Double]]): Number = fo match {
    case Some(_) => FuzzyNumber(nominalValue, factor, fo)
    case None => ExactNumber(nominalValue, factor)
  }

  /**
   * Make a copy of this Number, given the same degree of fuzziness as the original.
   * Only the nominalValue and factor will change.
   * This method should be followed by a call to specialize.
   *
   * @param v the nominalValue (a Double).
   * @param f  Factor.
   * @param fo optional fuzz.
   * @return either a Number.
   */
  def make(v: Double, f: Factor, fo: Option[Fuzziness[Double]]): Number = FuzzyNumber(Value.fromDouble(Some(v)), f, fo)
}

/**
 * The `FuzzyNumber` companion object provides operations and utilities for working with
 * fuzzy numbers, enabling probabilistic and inexact computations. It includes
 * methods for comparison, arithmetic, and adding fuzziness to numbers.
 */
object FuzzyNumber {

  val Ellipsis = "..."

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
    def same(p: Double)(x1: Number, x2: Number): Boolean = x1.doAdd(x2.makeNegative).isProbablyZero(p)
  }

  /**
   * For fuzzy numbers, it's appropriate to use the the normal mechanism for compare, even for NatLog numbers.
   *
   * NOTE, we first invoke same(p)(x, y) to determine if the Numbers are the same in a canonical manner.
   * However, we could actually skip this step and always just invoke the else part of the expression.
   *
   * @param x the first number.
   * @param y the second number.
   * @param p the probability criterion.
   * @return an Int representing the order.
   */
  def fuzzyCompare(x: Number, y: Number, p: Double): Int =
    if (implicitly[Fuzzy[Number]].same(p)(x, y)) 0
    else GeneralNumber.plus(x, Number.negate(y)).signum(p)

  //  /**
  //   * Creates a new fuzzy number using the provided value, factor, and optional fuzziness.
  //   * NOTE this method is not required to be explicitly declared.
  //   * However, it is useful for navigation (call hierarchy).
  //   *
  //   * @param v  the numerical value to be used for the fuzzy number.
  //   * @param f  the factor associated with the fuzzy number.
  //   * @param fo an optional fuzziness defining the tolerance or uncertainty of the fuzzy number.
  //   * @return a new instance of a fuzzy number initialized with the given parameters.
  //   */
  //  def apply(v: Value, f: Factor, fo: Option[Fuzziness[Double]]): Number = new FuzzyNumber(v, f, fo)

  /**
   * Method to construct an invalid Number.
   *
   * @return Number.apply()
   */
  def apply(): Number = Number.apply()

  /**
   * Method to add a FuzzyNumber and a (general) Number.
   *
   * @param x a FuzzyNumber.
   * @param y a Number.
   * @return the sum of x and y.
   */
  def plus(x: FuzzyNumber, y: Number): Number = x.alignFactors(y) match {
    case (a: GeneralNumber, b: GeneralNumber) =>
      val (p, q) = a.alignTypes(b)
      (p, q) match {
        case (n: FuzzyNumber, _) => composeDyadic(n, q, p.factor, DyadicOperationPlus, independent = true, None)
        case (_, n: FuzzyNumber) => composeDyadic(n, p, q.factor, DyadicOperationPlus, independent = true, None)
        case (_, _) => p doAdd q
      }
  }

  /**
   * Evaluate a dyadic operator, defined by op, on n and q.
   * Parameter independent relates to the calculation of the error bounds of the result.
   *
   * CONSIDER removing this method (merging it with code in GeneralNumber).
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
  private def composeDyadic(n: Number, q: Number, f: Factor, op: DyadicOperation, independent: Boolean, coefficients: Option[(Double, Double)]): Number = n match {
    case x: FuzzyNumber => prepareWithSpecialize(x.composeDyadicFuzzy(q, f)(op, independent, coefficients))
  }

  /**
   * Method to multiply two Numbers, the first a GeneralNumber.
   *
   * CONSIDER why do we have this as well as the times method in GeneralNumber?
   * Perhaps the type of x should be FuzzyNumber.
   *
   * @param x a GeneralNumber.
   * @param y a Number.
   * @return the product of x and y.
   */
  def times(x: GeneralNumber, y: Number): Number = x.alignFactors(y) match {
    case (a: GeneralNumber, b: GeneralNumber) =>
      val (p, q) = a.alignTypes(b)
      (p, q) match {
        case (n: FuzzyNumber, _) => composeDyadic(n, q, p.factor, DyadicOperationTimes, independent = x != y, None)
        case (_, n: FuzzyNumber) => composeDyadic(n, p, q.factor, DyadicOperationTimes, independent = x != y, None)
        case (_, _) => p doMultiply q
      }
  }

  /**
   * Method to add fuzz to a FuzzyNumber.
   *
   * @param n the Number to be fuzzied.
   * @param f the fuzziness to be added.
   * @return a fuzzied version of n.
   */
  def addFuzz(n: Number, f: Fuzziness[Double]): Number = (n.nominalValue, n.fuzz) match {
    case (v@Left(Left(Some(_))), fo) => addFuzz(n, v, fo, f)
    case _ => n
  }

  /**
   * Adds fuzziness to a given number.
   *
   * @param number      The input number to which fuzziness will be added.
   * @param v           The value component associated with the resulting fuzzy number.
   * @param fo          An optional initial fuzziness to combine with.
   * @param fAdditional Additional fuzziness to be applied.
   */
  private def addFuzz(number: Number, v: Value, fo: Option[Fuzziness[Double]], fAdditional: Fuzziness[Double]) = {
    val combinedFuzz = for (f <- fo.orElse(Some(AbsoluteFuzz(0.0, Box))); p <- number.toDouble; g <- Fuzziness.combine(p, 0, f.style, independent = false)((fo, fAdditional.normalize(p, f.style)))) yield g
    FuzzyNumber(v, number.factor, combinedFuzz)
  }

  /**
   * Raises a FuzzyNumber to the power of a given number.
   *
   * @param number the FuzzyNumber to be raised to a power.
   * @param p      the exponent to which the FuzzyNumber is raised.
   * @return the result of raising the FuzzyNumber to the specified power.
   */
  private def power(number: FuzzyNumber, p: Number): Number =
    composeDyadic(number.scale(PureNumber), p, p.factor, DyadicOperationPower, independent = false, getPowerCoefficients(number, p))

  /**
   * Get the fuzz coefficients for calculating the fuzz on a power operation.
   * According to the "Generalized Power Rule," these coefficients should be y and y ln x, respectively,
   * where x is the magnitude of n and y is the magnitude of p.
   *
   * @param n the number to be raised to power p.
   * @param p the power (exponent) with which to raise n.
   * @return the value of n to the power of p.
   */
  private def getPowerCoefficients(n: Number, p: Number): Option[(Double, Double)] =
    for (z <- n.toDouble; q <- p.toDouble) yield (q, q * math.log(z))

  /**
   *
   * @param p the confidence desired. Ignored if isZero is true.
   * @param f the fuzziness.
   * @param x the value to be tested (may be positive or negative).
   * @return true if x is within the tolerance range of f, given confidence level p. Otherwise, false
   */
  private def withinWiggleRoom(p: Double, f: Fuzziness[Double], x: Double) = f.normalizeShape.wiggle(p) > math.abs(x)
}

case class FuzzyNumberException(str: String) extends Exception(str)
