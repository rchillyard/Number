/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{negate, prepareWithSpecialize}
import com.phasmidsoftware.number.core.Operations.doTransformValueMonadic
import com.phasmidsoftware.number.core.Rational.toInts

import scala.annotation.tailrec
import scala.util._

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * TODO continue refactoring to merge similar methods, particularly in GeneralNumber and FuzzyNumber.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Radian, and NatLog.
  * @param fuzz   the (optional) fuzziness of this Number.
  */
abstract class GeneralNumber(val value: Value, val factor: Factor, val fuzz: Option[Fuzziness[Double]]) extends Number with Fuzz[Double] {

  self =>

  /**
    * Method to compare this Number with that Field.
    * Required by implementing Ordered[Field].
    *
    * @param that (a Field).
    * @return the comparison.
    */
  def compare(that: Field): Int = (this, that) match {
    case (x: FuzzyNumber, Real(y)) => x.compare(y)
    case (x: ExactNumber, Real(y)) => x.compare(y)
    case _ => that.compare(Real(this))
  }

  /**
    * Method to determine if this is a valid Number.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  def isValid: Boolean = maybeDouble.isDefined

  /**
    *
    * @return true if the value of this GeneralNumber is one.
    */
  def isUnity: Boolean = doSubtract(Number.one).isZero

  /**
    * Method to apply a function to this Number.
    *
    * @param f      a function Double=>Double.
    * @param dfByDx the first derivative of f.
    * @return a Try[Number] which is the result of applying f to this Number.
    */
  def applyFunc(f: Double => Double, dfByDx: Double => Double): Try[Number] = GeneralNumber.applyFunc(f, dfByDx)(this)

  /**
    * Method to get the value of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */
  def toDouble: Option[Double] = maybeDouble

  /**
    * Method to get the value of this Number as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.convertDouble(x).
    *
    * @return an Option of Rational.
    */
  def toRational: Option[Rational] = maybeRational

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int] = maybeInt

  /**
    * Method to get the value of this Number as an (optional) BigInt.
    * This will return Some(x) only if this is an Int, or a Rational with unit denominator.
    *
    * @return an Option of BigInt.
    */
  def toBigInt: Option[BigInt] = value match {
    case Right(x) => Some(BigInt(x))
    case Left(Right(r)) if r.isWhole => Some(r.n)
    case _ => None
  }

  /**
    * Method to get the value of this Number as an (optional) BigInt.
    * This will return Some(x) only if this is an Int, or a Rational with unit denominator.
    *
    * TESTME
    *
    * @return an Option of BigDecimal.
    */
  def toBigDecimal: Option[BigDecimal] = value match {
    case Right(x) => Some(BigDecimal(x))
    case Left(Right(r)) => r.toBigDecimal
    case Left(Left(Some(x))) => Some(BigDecimal(x))
    case _ => None
  }

  /**
    * @return true if this Number is equal to zero.
    */
  def isInfinite: Boolean = GeneralNumber.isInfinite(this)

  /**
    * Method to determine if this Number is positive.
    * Use case: does the String representation not start with a "-"?
    *
    * CONSIDER evaluating toString instead.
    *
    * @return true if this Number is greater than or equal to 0.
    */
  def isPositive: Boolean = signum >= 0

  /**
    * Method to determine if this Number is actually represented as an Integer.
    *
    * @return true if exact and rational.
    */
  def isInteger: Boolean = value match {
    case Right(_) => true
    case _ => false
  }

  /**
    * Method to determine if this Number is actually represented as a Rational.
    *
    * @return true if exact and rational.
    */
  def isRational: Boolean = value match {
    case Left(Right(_)) => true
    case _ => false
  }

  /**
    * Method to determine if this is an imaginary Number,
    * that's to say a number with negative value and Root2 as its factor.
    *
    * @return true if imaginary.
    */
  def isImaginary: Boolean = factor match {
    case Root2 if Value.signum(value) < 0 => true
    case _ => false
  }

  /**
    * Negative of this Number.
    */
  def makeNegative: Number = negate(this) //doMultiply(Number(-1))

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e. this/n.
    */
  def doDivide(n: Number): Number = doMultiply(Number.inverse(n))

  /**
    * Yields the square root of this Number.
    * The result will be exact.
    */
  def sqrt: Number = Number.sqrt(this)

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the sine of this.
    */
  def sin: Number = Number.sin(this)

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the cosine.
    */
  def cos: Number = Number.negate(negate(scale(Radian)) doAdd Number(Rational.half, Radian).makeNegative).sin

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number = Number.atan(this, y)

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the natural log of this.
    */
  def log: Number = Number.log(this)

  /**
    * Method to raise NatLog to the power of this number.
    * The result will be a Number with NatLog factor.
    *
    * @return the e to the power of this.
    */
  def exp: Number = Number.exp(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int = Number.signum(this)

  /**
    * Method to yield the absolute value of this Number.
    *
    * @return this if its positive, else - this.
    */
  def abs: Number = if (signum >= 0) this else makeNegative

  /**
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    * In other words,  in the case where f is not factor, the numerical value of the result's value will be different
    * from this value.
    *
    * @param f the new factor for the result.
    * @return a Number based on this and factor.
    */
  def scale(f: Factor): Number = Number.scale(this, f).specialize

  /**
    * Action to render this GeneralNumber as a String.
    *
    * @return a String.
    */
  def render: String = simplify.toString

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * NOTE: This method is used, although it doesn't appear so.
    *
    * @param other the Number to be compared with.
    * @param p     the confidence expressed as a fraction of 1 (0.5 would be a typical value).
    * @return -1, 0, 1 as usual.
    */
  def fuzzyCompare(other: Number, p: Double): Int = FuzzyNumber.fuzzyCompare(this, other, p)

  /**
    * Method to derive a fuzziness that covers the discrepancy between this and other.
    *
    * @param other another Number: the ideal or target value.
    * @return an optional relative Fuzziness.
    */
  def asComparedWith(other: Number): Option[Fuzziness[Double]] =
    for {
      q <- doSubtract(other).toDouble
      r <- other.scale(Scalar).toDouble
      p <- AbsoluteFuzz(math.abs(q) / 2, Box).relative(r)
    } yield p

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadic(other: Number, f: Factor)(op: DyadicOperation): Option[Number] = doComposeDyadic(other, f)(op.functions)

  /**
    * Evaluate a monadic operator on this.
    *
    * CONSIDER the exact cases don't change the fuzziness at all.
    * But, if there is already fuzziness, then the monadic function should change it.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def transformMonadic(f: Factor)(op: MonadicOperation): Option[Number] =
    Operations.doTransformValueMonadic(value)(op.functions) flatMap {
      case v@Right(_) => Some(make(v, f))
      case v@Left(Right(_)) => Some(make(v, f))
      case v => make(v, f) match {
        case n: GeneralNumber => for (t <- toDouble; x <- n.toDouble) yield n.make(Fuzziness.monadicFuzziness(op, t, x, fuzz))
      }
    }

  /**
    * Evaluate a query operator on this.
    *
    * @param op the appropriate QueryOperation.
    * @return a Boolean.
    */
  def query[T](op: QueryOperation[T], defaultVal: => T): T = Operations.doQuery(value, op.getFunctions).getOrElse(defaultVal)

  /**
   * Method to determine if this Number can stay "as is" under transformation according to context.
    *
   * @param context an optional Factor.
   * @return true if context is empty or contains this factor.
    */
  protected def factorAsIs(context: Context): Boolean =
    context.isEmpty || context.contains(factor) // CONSIDER using forAll

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the factor will change.
    * This method does not need to be followed by a call to specialize.
    *
    * @param f the factor.
    * @return a Number.
    */
  def make(f: Factor): Number = make(value, f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return a Number.
    */
  def make(v: Value): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return a Number.
    */
  def make(v: Int, f: Factor): Number = make(Value.fromInt(v), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return a Number.
    */
  def make(v: Int): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * NOTE that this method is primarily used by doComposeDyadic
    *
    * @param r a Rational.
    * @param f Factor.
    * @return a Number.
    */
  def make(r: Rational, f: Factor): Number = make(Value.fromRational(r), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param r the value.
    * @return a Number.
    */
  def make(r: Rational): Number = make(r, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * TESTME
    *
    * @param x the value (a Double).
    * @param f Factor.
    * @return a Number.
    */
  def make(x: Double, f: Factor): Number = make(Value.fromDouble(Some(x)), f)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar and also to force
    * any fuzziness to be absolute.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  def normalize: Field = {
    val z: Field = factor match {
      case Scalar => Real(this)
      case r@Root(_) if Value.signum(value) < 0 => GeneralNumber.normalizeRoot(value, r)
      case Radian => Real(this) // Number.modulate(this) NOTE: we do modulation at other times
      case _ => Real(scale(Scalar))
    }
    z match {
      case Real(fuzzyNumber: FuzzyNumber) => Real(fuzzyNumber.normalizeFuzz)
      case r@Real(_) => r
      case x: Complex => x
      case x => throw NumberException(s"normalize problem: $x")
    }
  }

  /**
    * Method to ensure that the value is within some factor-specific range.
    * In particular, Radian=based numbers are modulated to the range 0..2
    *
    * @return this or an equivalent Number.
    */
  def modulate: Number = Number.modulate(this)

  /**
    * Return a Number which uses the most restricted type possible.
    * A Number based on a Double will yield a Number based on a Rational (if the conversion is exact).
    * A Number based on a Rational will yield a Number based on a BigInt (if there is a unit denominator).
    * A Number based on a BigInt will yield a Number based on a Int (if it is sufficiently small).
    *
    * @return a Number with the same magnitude as this.
    */
  //protected
  def specialize: Number = value match {
    // XXX Int case
    case Right(_) => this
    // XXX Rational case
    case Left(Right(r)) =>
      Try(r.toInt) match {
        case Success(b) => make(b).specialize
        case _ => this
      }
    case Left(Left(Some(x))) if x.isNaN => Number.NaN
    // XXX Double case
    case d@Left(Left(Some(x))) =>
      // NOTE: here we attempt to deal with Doubles.
      // If a double can be represented by a BigDecimal with scale 0, 1, or 2 then we treat it as exact.
      // Otherwise, we will give it appropriate fuzziness.
      // In general, if you wish to have more control over this, then define your input using a String.
      // CONSIDER will this handle numbers correctly which are not close to 1?
      Rational.createExact(x) match {
        case Success(r) =>
          r.toBigDecimal.map(_.scale) match {
            case Some(0) | Some(1) | Some(2) => make(r).specialize
            // CONSIDER in following line adding fuzz only if this Number is exact.
            case Some(n) => FuzzyNumber(d, factor, fuzz).addFuzz(AbsoluteFuzz(Fuzziness.toDecimalPower(5, -n), Box))
            case _ => FuzzyNumber(d, factor, fuzz).addFuzz(Fuzziness.doublePrecision)
          }
        case Failure(_) => FuzzyNumber(d, factor, fuzz).addFuzz(Fuzziness.doublePrecision)
      }
    // XXX Invalid case
    case _ => this
  }

  /**
    * Make a copy of this FuzzyNumber but with additional fuzz given by f.
    *
    * @param f the additional fuzz.
    * @return this but with fuzziness which is the convolution of fuzz and f.
    */
  def addFuzz(f: Fuzziness[Double]): Number = FuzzyNumber.addFuzz(this, f)

  /**
    * Method to align the factors of this and x such that the resulting Numbers (in the tuple) each have the same factor.
    *
    * @param x the Number to be aligned with this.
    * @return a tuple of two Numbers with the same factor.
    */
  //protected
  def alignFactors(x: Number): (Number, Number) = factor match {
    case Scalar => (this, x.scale(factor))
    case _ => (scale(x.factor), x)
  }

  /**
    * Method to align the types of this and x such that the resulting Numbers (in the tuple) each have the same structure.
    *
    * CONSIDER renaming this alignValueTypes
    *
    * @param q the Number to be aligned with this.
    * @return a tuple of two Numbers, the first of which will be the more general type:
    *         (Invalid vs. Double, Double vs. Rational, Rational vs. Int).
    */
  //protected
  def alignTypes(q: Number): (Number, Number) = q match {
    case x: GeneralNumber =>
      value match {
        // XXX this is an invalid Number: return a pair of invalid numbers
        case Left(Left(None)) => (this, this)
        // XXX this value is a real Number: convert x to a Number based on real.
        case Left(Left(Some(_))) => x.value match {
          // XXX x's value is invalid: swap the order so the the first element is invalid
          case Left(Left(None)) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as a Double
          case _ => (this, Number.prepare(x.maybeDouble.map(y => make(y, x.factor, x.fuzz).specialize)))
        }
        // XXX this value is a Rational:
        case Left(Right(_)) => x.value match {
          // XXX x's value is a real Number: swap the order so that the first element is the real number
          case Left(Left(_)) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as a Rational
          case _ => (this, x.make(x.maybeRational.getOrElse(Rational.NaN)).specialize)
        }
        // XXX this value is an Int:
        case Right(_) => x.value match {
          // XXX x's value is a BigInt, Rational or real Number: swap the order so that the first element is the BigInt/Rational/real number
          case Left(_) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as an Int
          case _ => (this, x.make(x.maybeInt.getOrElse(0), x.factor).specialize)
        }
      }
  }

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using query
    */
  def maybeRational: Option[Rational] = Value.maybeRational(value)

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    * If
    *
    * @return Some(x) if value can be converted properly to a Double, otherwise None.
    */
  def maybeDouble: Option[Double] = Value.maybeDouble(value)

  /**
    * Method to return this ExactNumber as a Real.
    *
    * @return Some(Real(this)).
    */
  def asReal: Option[Real] = Some(Real(this))

  /**
    * Ensure that this is consistent with hashCode.
    *
    * @param other the other Any.
    * @return true if this and Any are, logically the same.
    */
  override def equals(other: Any): Boolean = other match {
    case that: GeneralNumber =>
      (that canEqual this) &&
          value == that.value &&
          factor == that.factor &&
          fuzz == that.fuzz
    case _ => false
  }

  /**
    * TESTME
    *
    * @return
    */
  override def hashCode(): Int = {
    val state = Seq(value, factor, fuzz)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  /**
    * Evaluate a dyadic operator on this and other, using the various functions passed in.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other     the other operand, a Number.
    * @param f         the factor to apply to the result.
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  private def doComposeDyadic(other: Number, f: Factor)(functions: DyadicFunctions): Option[Number] = {
    val vo: Option[Value] = Operations.doComposeValueDyadic(value, other.value)(functions)
    for (v <- vo) yield make(v, f) // CONSIDER what about extra fuzz?
  }

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    */
  private lazy val maybeInt: Option[Int] = Value.maybeInt(value)

  /**
    * CONSIDER do we really need this?
    */
  private def canEqual(other: Any): Boolean = other.isInstanceOf[GeneralNumber]
}

object GeneralNumber {

  def applyFunc(f: Double => Double, dfByDx: Double => Double)(x: Number): Try[Number] = {
    val op: MonadicOperation = MonadicOperationFunc(f, dfByDx)
    val no: Option[Number] = Operations.doTransformValueMonadic(x.value)(op.functions) flatMap {
      v =>
        x.make(v, Scalar) match {
          case n: GeneralNumber =>
            for (t <- x.toDouble; z <- n.toDouble) yield n.make(Fuzziness.monadicFuzziness(op, t, z, x.fuzz))
        }
    }
    FP.toTry(no, Failure(NumberException("applyFunc: logic error")))
  }

  def isZero(x: Number): Boolean = x.query(QueryOperationIsZero, false)

  def isInfinite(x: Number): Boolean = x.query(QueryOperationIsInfinite, false)

  def plus(x: Number, y: Number): Number = x match {
    case ExactNumber(Right(0), Scalar) => y
    case z: GeneralNumber =>
      if (y == ExactNumber(Right(0), Scalar)) x
      else {
        val (a, b) = z.alignFactors(y)
        a.factor match {
          case Logarithmic(_) => plusAligned(a.scale(Scalar), b.scale(Scalar))
          case Root(_) => plusAligned(a.scale(Scalar), b.scale(Scalar))
          case _ => plusAligned(a, b)
        }
      }
  }

  @tailrec
  def times(x: Number, y: Number): Number = x match {
    case Number.zero => Number.zero
    case Number.one => y
    case a: GeneralNumber =>
      y match {
        case Number.zero => Number.zero
        case Number.one => x
        case n@FuzzyNumber(_, _, _) => n doMultiply x
        case z: GeneralNumber =>
          val (p, q) = a.alignTypes(z)
          (p.factor, q.factor) match {
            case (PureNumber(_), Scalar) => doTimes(p, q, p.factor)
            case (Scalar, PureNumber(_)) => doTimes(p, q, q.factor)
            case (f: Logarithmic, g: Logarithmic) if f == g =>
              prepareWithSpecialize(p.composeDyadic(q, f)(DyadicOperationPlus))
            case (f: Logarithmic, Scalar) if q.signum > 0 => prepareWithSpecialize(p.composeDyadic(q.scale(f), f)(DyadicOperationPlus))
            case (_: Logarithmic, Scalar) => times(p.scale(Scalar), q)
            case (Root(m), Root(n)) if m == 2 && n == 2 =>
              GeneralNumber.doTimes(p, q, p.factor)
            case (Root(_), Root(_)) if p == q => p.make(Scalar)
            case (Root(_), Root(_)) => doTimes(p, q.scale(p.factor), p.factor)
            case _ => times(p.scale(Scalar), q.scale(Scalar))
          }
      }
  }
//  def times(x: Number, y: Number): Number = (x, y) match {
//    case (_, Number.zero) | (Number.zero, _) => Number.zero
//    case (a, Number.one) => a
//    case (Number.one, b) => b
//    case (a: ExactNumber, b: FuzzyNumber) => b doMultiply a
//    case (a: FuzzyNumber, b) => a doMultiply b
//    case (a: ExactNumber, b: ExactNumber) => ExactNumber.product(a, b)
//    case _ => throw NumberException(s"GeneralNumber.times($x, $y): no match")
//  }

//    x match {
//    case ExactNumber(Right(0), Scalar) => Number.zero
//    case ExactNumber(Right(1), Scalar) => y
//    case a: GeneralNumber =>
//      y match {
//        case ExactNumber(Right(0), Scalar) => Number.zero
//        case ExactNumber(Right(1), Scalar) => x
//        case n@FuzzyNumber(_, _, _) => n doMultiply x
//        case z: GeneralNumber =>
//          val (p, q) = a.alignTypes(z)
//          (p.factor, q.factor) match {
//            case (PureNumber(_), Scalar) => doTimes(p, q, p.factor)
//            case (Scalar, PureNumber(_)) => doTimes(p, q, q.factor)
//            case (f: Logarithmic, Scalar) if q.signum > 0 => prepareWithSpecialize(p.composeDyadic(q.scale(f), f)(DyadicOperationPlus))
//            case (_: Logarithmic, Scalar) => times(p.scale(Scalar), q)
//            case (Root(_), Root(_)) if p == q => p.make(Scalar)
//            case (Root(_), Root(_)) => doTimes(p, q.scale(p.factor), p.factor)
//            case _ => times(p.scale(Scalar), q.scale(Scalar))
//          }
//      }
//  }

  /**
    * Method to raise an (exact) Number to a power.
    * NOTE: This method is invoked only by doPower (in ExactNumber).
    *
    * @param x the base Number (always exact).
    * @param y the power (may not be exact).
    * @return x raised to the power of y.
    */
  def power(x: Number, y: Number): Number =
    y.scale(Scalar).toRational match {
      case Some(r) => power(x, r).specialize
      case None =>
        // NOTE this is not used, but it doesn't seem to handle fuzziness (of the exponent) properly either.
        val zo = for (p <- x.toDouble; q <- y.toDouble) yield Number(math.pow(p, q))
        prepareWithSpecialize(zo)
    }

  /**
    * Method to raise an (exact) Number to a Rational power.
    *
    * @param x the base Number.
    * @param r the power.
    * @return an exact Number (CHECK is that correct?)
    */
  @tailrec
  private def power(x: Number, r: Rational): Number = if (r.isZero) Number.one
  else if (r.isUnity || x == Number.one) x
  else
    x.factor match {
      case Logarithmic(_) =>
        doTransformValueMonadic(x.value)(MonadicOperationScale(r).functions) match {
          case Some(v) => x.make(v)
          case None => throw NumberException("power: logic error")
        }

      case Radian =>
        power(x.scale(Scalar), r)

      case Scalar =>
        toInts(r) match {
          case Some((n, d)) =>
            root(power(x, n), d) match {
              case Some(q) => q.simplify
              case None => Number(r.toDouble)
            }
          case _ =>
            throw NumberException("rational power cannot be represented as two Ints")
        }

      // TODO we should also handle some situations where r.d is not 1.
      case Root(n) if r.n == n && r.d == 1 => x.make(Scalar)
      case _ => power(x.scale(Scalar), r)

    }

  /**
    * Method to take the ith root of n.
    *
    * @param n the Number whose root is required.
    * @param i the ordinal of the root (2: square root, etc.).
    * @return the root.
    */
  private def root(n: Number, i: Int): Option[Number] = i match {
    case 0 => throw NumberException(s"root: logic error: cannot take ${i}th root")
    case 1 => Some(n)
    case 2 => Some(n.make(Root2))
    case 3 => Some(n.make(Root3)) // TESTME
    case _ => None
  }

  private def power(n: Number, i: Int) = i match {
    case x if x > 0 => LazyList.continually(n).take(x).product
    case x => LazyList.continually(Number.inverse(n)).take(-x).product
  }

  def doTimes(p: Number, q: Number, factor: Factor): Number = {
    val maybeNumber = p.composeDyadic(q, factor)(DyadicOperationTimes)
    prepareWithSpecialize(maybeNumber)
  }

  private def plusAligned(x: Number, y: Number): Number = (x, y) match {
    case (a: GeneralNumber, b: GeneralNumber) =>
      y match {
        case n@FuzzyNumber(_, _, _) => n doAdd x
        case _ =>
          val (p, q) = a.alignTypes(b)
          prepareWithSpecialize(p.composeDyadic(q, p.factor)(DyadicOperationPlus))
      }
  }

  private def normalizeRoot(value: Value, r: Root) = {
    Operations.doTransformValueMonadic(value)(MonadicOperationNegate.functions) match {
      case Some(q) => ComplexCartesian(Number.zero, ExactNumber(q, r).scale(Scalar))
      case None => throw NumberException("GeneralNumber.normalizeRoot: logic error")
    }
  }
}