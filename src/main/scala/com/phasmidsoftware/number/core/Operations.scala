package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.{fail, toTryWithThrowable, tryF, tryMap}

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.Ordered.orderingToOrdered
import scala.util._

/**
  * This module relates primarily to operations on Value (a type defined in package.scala).
  */

/**
  * Definitions of MonadicOperations.
  */
sealed trait MonadicOperation {
  /**
    * Method to yield a set of functions which can be applied to Int, Rational, or Double values
    * for this MonadicOperation.
    *
    * @return a set of three functions: Int=>Try[Int], Rational=>Try[Rational], Double=>Try[Double].
    */
  val functions: MonadicFunctions

  /**
    * Yield a function which provides the derivative of this MonadicOperation, i.e. f(x), with respect to x, at x.
    * The dimensions of the derivative are those of f / x.
    *
    * @return a function Double => Double
    */
  val derivative: Double => Double

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean
}

/**
  * MonadicOperation to negate a GeneralNumber.
  */
case object MonadicOperationNegate extends MonadicOperation {
  val functions: MonadicFunctions = {
    val fInt = tryF[Int, Int](math.negateExact)
    val fRational = tryF[Rational, Rational](implicitly[Numeric[Rational]].negate)
    val fDouble = tryF[Double, Double](implicitly[Numeric[Double]].negate)
    (fInt, fRational, fDouble)
  }

  val derivative: Double => Double = _ => -1

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = true // not used
}

/**
  * MonadicOperation to invert a GeneralNumber.
  */
case object MonadicOperationInvert extends MonadicOperation {
  def invertInt(x: Int): Try[Int] = x match {
    case 1 => Success(1)
    case _ => Failure(NumberException("can't invert Int"))
  }

  private val xf: Fractional[Double] = implicitly[Fractional[Double]]

  def invertDouble(x: Double): Try[Double] = Try(xf.div(xf.one, x))

  val functions: MonadicFunctions = (invertInt, tryF[Rational, Rational](x => x.invert), invertDouble)

  val derivative: Double => Double = x => -1.0 / x / x

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = true // not used
}

/**
  * MonadicOperation to raise e (Euler's number) to the power of a GeneralNumber.
  */
case object MonadicOperationExp extends MonadicOperation {
  def expDouble(x: Double): Try[Double] = Try(Math.exp(x))

  val expRat: Rational => Try[Rational] = x =>
    Rational.toInt(x) map {
      case 0 => Rational.one
      case 1 => Rational(math.E)
      case y => Rational(Math.exp(y))
    }

  val functions: MonadicFunctions = (
    fail("can't do exp Int=>Int"),
    fail("can't do exp Rational=>Rational"),
    expDouble)

  val derivative: Double => Double = x => Math.exp(x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false
}

/**
  * MonadicOperation to yield the natural logarithm of a GeneralNumber.
  */
case object MonadicOperationLog extends MonadicOperation {
  def expDouble(x: Double): Try[Double] = Try(Math.log(x))

  val expRat: Rational => Try[Rational] = x =>
    Rational.toInt(x) map {
      case 0 => Rational(-1, 0)
      case y => Rational(Math.log(y))
    }

  val functions: MonadicFunctions = (
    fail("can't do exp Int=>Int"),
    expRat,
    expDouble)

  val derivative: Double => Double = x => 1 / x

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false //not used
}

/**
  * MonadicOperation to calculate the sine of a GeneralNumber.
  */
case object MonadicOperationSin extends MonadicOperation {
  def sinDouble(x: Double): Try[Double] = Try(Math.sin(x * math.Pi))

  val sinRat: Rational => Try[Rational] = x =>
    if (!x.invert.isWhole) sinDouble(x.toDouble).map(Rational(_))
    else x.invert.toInt match {
      case 6 => Success(Rational.half)
      case 4 => Rational.half.sqrt
      case 3 => Rational(3).sqrt map (_ / 2)
      case 2 => Success(Rational.one)
      case _ => Failure(NumberException("sine cannot be Rational"))
    }

  val functions: MonadicFunctions = (tryF[Int, Int](_ => 0), sinRat, sinDouble)

  val derivative: Double => Double = x => math.cos(x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false
}

/**
  * MonadicOperation to yield the arctangent a GeneralNumber.
  *
  * @param sign an Int which will distinguish between results in the four quadrants.
  */
case class MonadicOperationAtan(sign: Int) extends MonadicOperation {
  def atan(x: Double): Try[Double] =
    Try(math.atan2(x, sign) / math.Pi) // TODO use scale // TEST me

  val atanRat: Rational => Try[Rational] = x =>
    if (!x.isWhole) atan(x.toDouble).map(Rational(_))
    else x.toInt match {
      case -1 => Success(Rational.one * 3 / 4 + (if (sign < 0) Rational.one else Rational.zero))
      case 0 => Success(Rational.zero + (if (sign < 0) Rational.one else Rational.zero))
      case 1 => Success(Rational.one / 4 + (if (sign < 0) Rational.one else Rational.zero))
      case _ => Failure(NumberException("atan cannot be Rational"))
    }

  val functions: MonadicFunctions = (fail("atan cannot be Rational"), atanRat, atan)

  val derivative: Double => Double = x => 1 / (1 + x * x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false
}

/**
  * MonadicOperation to yield the modulus a GeneralNumber.
  */
case object MonadicOperationModulate extends MonadicOperation {
  private def modulate[X: Numeric](z: X, min: X, max: X): X = {
    val nx = implicitly[Numeric[X]]

    @tailrec
    def inner(result: X): X =
      if (result < min) inner(nx.plus(result, nx.plus(max, nx.negate(min))))
      else if (result > max) inner(nx.plus(result, nx.plus(min, nx.negate(max))))
      else result

    inner(z)
  }

  val functions: MonadicFunctions = (
    tryF(z => modulate(z, 0, 2)),
    tryF(z => modulate(z, Rational.zero, Rational.two)),
    tryF(z => modulate(z, 0, 2))
  )

  val derivative: Double => Double = _ => 1

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = true // not used
}

/**
  * MonadicOperation to yield the square root of a GeneralNumber.
  *
  * CONSIDER eliminating this and using power only.
  */
case object MonadicOperationSqrt extends MonadicOperation {
  val sqrtInt: Int => Try[Int] =
    x => toTryWithThrowable(Rational.squareRoots.get(x), NumberException("Cannot create Int from Double"))

  val functions: MonadicFunctions = (sqrtInt, x => x.sqrt, tryF(x => math.sqrt(x)))

  val derivative: Double => Double = x => 1 / math.sqrt(x) / 2

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false // not used
}

/**
  * This monadic operation is used to scale a Value by an Int.
  *
  * @param r the scale factor (a Rational).
  */
case class MonadicOperationScale(r: Rational) extends MonadicOperation {
  private val c: Double = r.toDouble

  val functions: MonadicFunctions = {
    val fInt = if (r.isWhole) tryF[Int, Int](math.multiplyExact(_, r.toInt)) else fail("can't do scale function Int=>Int")
    val fRational = tryF[Rational, Rational](_ * r)
    val fDouble = tryF[Double, Double](_ * c)
    (fInt, fRational, fDouble)
  }

  val derivative: Double => Double = _ => c

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false // not used
}

/**
  * Trait to define a dyadic operation.
  */
sealed trait DyadicOperation {
  val functions: DyadicFunctions

  val absolute: Boolean
}

case object DyadicOperationPlus extends DyadicOperation {
  val functions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.addExact)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].plus)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].plus)
    (fInt, fRational, fDouble)
  }

  val absolute: Boolean = true
}

case object DyadicOperationTimes extends DyadicOperation {
  val functions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.multiplyExact)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].times)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].times)
    (fInt, fRational, fDouble)
  }

  val absolute: Boolean = false
}

case object DyadicOperationPower extends DyadicOperation {
  val functions: DyadicFunctions = {
    val fInt = (x: Int, p: Int) => Rational.narrow(BigInt(x).pow(p), Int.MinValue, Int.MaxValue).map(_.toInt)
    val fRational = (x: Rational, p: Rational) => x.power(p)
    val fDouble = tryF[Double, Double, Double]((x, p) => math.pow(x, p))
    (fInt, fRational, fDouble)
  }

  val absolute: Boolean = false
}

/**
  * Definitions of QueryOperations.
  */
sealed trait QueryOperation {
  def getFunctions: QueryFunctions
}

case object QueryOperationIsZero extends QueryOperation {
  def getFunctions: QueryFunctions = {
    val fInt = tryF[Int, Boolean](x => x == 0)
    val fRational = tryF[Rational, Boolean](x => x.signum == 0)
    val fDouble = tryF[Double, Boolean](x => x.sign == 0 || x.sign == -0)
    (fInt, fRational, fDouble)
  }
}

case object QueryOperationIsInfinite extends QueryOperation {
  def getFunctions: QueryFunctions = {
    val fInt = tryF[Int, Boolean](_ => false)
    val fRational = tryF[Rational, Boolean](x => x.isInfinity)
    val fDouble = tryF[Double, Boolean](x => x == Double.PositiveInfinity || x == Double.NegativeInfinity)
    (fInt, fRational, fDouble)
  }
}

object Operations {
  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    * The result is an Option[Value] rather than a GeneralNumber, as in GeneralNumber's doComposeMonadic.
    *
    * @param functions the tuple of four conversion functions.
    * @return an Option[Value] which is result of applying the appropriate function to the given value.
    */
  def doTransformValueMonadic(value: Value)(functions: MonadicFunctions): Option[Value] = {
    val (fInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Value] = {
      case Some(x) => Try(Value.fromDouble(fDouble(x).toOption))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Value] = e => tryMap(e)(x => for (r <- fRational(x)) yield Value.fromRational(r), xToZy0)
    tryMap(value)(x => for (i <- fInt(x)) yield Value.fromInt(i), xToZy1).toOption
  }

  def doQuery(v: Value, functions: QueryFunctions): Option[Boolean] = {
    val (fInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Boolean] = {
      case Some(n) => fDouble(n)
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Boolean] = y => tryMap(y)(x => fRational(x), xToZy0)
    tryMap(v)(x => fInt(x), xToZy1).toOption
  }

}