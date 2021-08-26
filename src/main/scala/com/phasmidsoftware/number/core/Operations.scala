package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.{fail, toTryWithThrowable, tryF, tryMap}

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.BigInt
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

  /**
    * Determine if, given an exact Rational (or Int) output value, this MonadicOperation returns an exact result.
    *
    * NOTE: it seems that all implementations of this method simply return true.
    * CONSIDER eliminate.
    *
    * @param r the input value (a Rational).
    * @return true if the result is exact.
    */
  def isExact(r: Rational): Boolean

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int
}

/**
  * MonadicOperation to negate a Number.
  *
  * NOTE: not valid on NatLog-scaled values.
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

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (ignored).
    * @return true.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to invert a Number.
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

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (ignored).
    * @return true.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to raise e (Euler's number) to the power of a Number.
  */
case object MonadicOperationExp extends MonadicOperation {
  def expInt(x: Int): Try[Int] = x match {
    case 0 => Success(1)
    case _ => Failure(NumberException("can't exp Int"))
  }

  val expRat: Rational => Try[Rational] = {
    case r if r.isInfinity && r.signum < 0 => Success(Rational.zero)
    case r => fail("can't do exp Rational=>Rational for non-zero parameter")(r)
  }

  def expDouble(x: Double): Try[Double] = Try(Math.exp(x))

  val functions: MonadicFunctions = (
    expInt,
    expRat,
    expDouble)

  val derivative: Double => Double = x => Math.exp(x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (a Rational).
    * @return the success status of the result of expInt or expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to yield the natural logarithm of a Number.
  */
case object MonadicOperationLog extends MonadicOperation {

  def logInt(x: Int): Try[Int] = x match {
    case 1 => Success(0)
    case _ => Failure(NumberException("can't log Int"))
  }

  private val logRat: Rational => Try[Rational] = {
    case Rational.zero => Success(Rational.infinity.negate)
    case r => fail("can't do log Rational=>Rational for parameter")(r)
  }

  def logDouble(x: Double): Try[Double] = Try(Math.log(x))

  val functions: MonadicFunctions = (
    logInt,
    logRat,
    logDouble)

  val derivative: Double => Double = x => 1 / x

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false //not used

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (a Rational).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to calculate the sine of a Number.
  * The number is a factor of pi, i.e. it is in radians.
  *
  * See https://en.wikipedia.org/wiki/List_of_trigonometric_identities
  */
case object MonadicOperationSin extends MonadicOperation {
  private val sinInt: Int => Try[Int] = tryF(_ => 0)

  private val (two, six) = (BigInt(2), BigInt(6))

  private val sinRatExact: Rational => Try[Rational] = x =>
    (x.n, x.d) match {
      case (n, `two`) if n.isValidInt => n.toInt match {
        case 1 => Success(Rational.one)
        case 3 => Success(Rational.one.negate)
        case _ => Failure(NumberException("sine cannot be exact Rational"))
      }
      case (n, `six`) if n.isValidInt => n.toInt match {
        case 1 | 5 => Success(Rational.half)
        case 7 | 11 => Success(Rational.half.negate)
        case _ => Failure(NumberException("sine cannot be exact Rational"))
      }
      case _ => Failure(NumberException("sine cannot be exact Rational"))
    }

  private val sinRatInexact: Rational => Try[Rational] = x =>
    // CONSIDER do we really need this first clause?
    if (!x.invert.isWhole) sinDouble(x.toDouble).map(Rational(_))
    else x.invert.toInt match {
      // CONSIDER eliminating the following...
      case 4 => Rational.half.sqrt
      case 3 => Rational(3).sqrt map (_ / 2)
      case _ => Failure(NumberException("sine cannot be Rational"))
    }

  private def sinDouble(x: Double): Try[Double] = Try(Math.sin(x * math.Pi))

  val functions: MonadicFunctions = (sinInt, r => sinRatExact(r) orElse sinRatInexact(r), sinDouble)

  val derivative: Double => Double = x => math.cos(x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (a Rational).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to yield the arctangent of a Number.
  *
  * @param sign an Int which will distinguish between results in the four quadrants.
  */
case class MonadicOperationAtan(sign: Int) extends MonadicOperation {

  val atanRat: Rational => Try[Rational] = r => {
    val flip = r.signum < 0
    (r.abs match {
      case Rational.infinity => Success(Rational.half)
      case Rational.zero => Success(Rational.zero)
      case Rational.one => Success(Rational.one / 4)
      case _ => Failure(NumberException("atan cannot be Rational"))
    }) map {
      r => if (flip) -r else r
    } map (
      r => if (sign < 0) Rational.one + r else r
      )
  }

  def atan(x: Double): Try[Double] =
    Try {
      math.atan2(x, sign) / math.Pi
    } // TODO use scale // TEST me

  val functions: MonadicFunctions = (fail("atan cannot be Int"), atanRat, atan)

  val derivative: Double => Double = x => 1 / (1 + x * x)

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (a Rational).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 4
}

/**
  * MonadicOperation to yield the modulus a Number.
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

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (ignored).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to yield the square root of a Number.
  *
  * CONSIDER eliminating this and using power only.
  */
case object MonadicOperationSqrt extends MonadicOperation {
  val sqrtInt: Int => Try[Int] = // CONSIDER not using squareRoots: there are other ways.
    x => toTryWithThrowable(Rational.squareRoots.get(x), NumberException("Cannot create Int from Double"))

  val sqrtRat: Rational => Try[Rational] = x => FP.toTry(x.root(2), Failure(NumberException("Cannot get exact square root")))

  val functions: MonadicFunctions = (sqrtInt, sqrtRat, tryF(x => math.sqrt(x)))

  val derivative: Double => Double = x => 1 / math.sqrt(x) / 2

  /**
    * True if fuzziness is to be considered absolute.
    */
  val absolute: Boolean = false // not used

  /**
    * Determine if, given an exact Rational (or Int) output value, this MonadicOperation returns an exact result.
    *
    * @param r the output value (ignored).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true
  //    s"isExact: $r" !! toInt(r).flatMap(sqrtInt).orElse(sqrtRat(r)).isSuccess

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 3
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

  /**
    * Determine if, given an exact Rational (or Int) input, this MonadicOperation returns an exact result.
    *
    * @param r the input value (ignored).
    * @return the success status of the result of expRat.
    */
  def isExact(r: Rational): Boolean = true

  /**
    * Relative precision, as used by createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to apply a generic function to a Number.
  *
  */
case class MonadicOperationFunc(f: Double => Double, dfByDx: Double => Double) extends MonadicOperation {
  val functions: MonadicFunctions = (fail("no apply"), fail("no apply"), tryF(f))

  val derivative: Double => Double = dfByDx

  val absolute: Boolean = true // not used

  def isExact(r: Rational): Boolean = false

  val fuzz: Int = 1
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
    val fRational = (x: Rational, p: Rational) => x.power(p)
    val fDouble = tryF[Double, Double, Double]((x, p) => math.pow(x, p))
    (powerInt, fRational, fDouble)
  }

  val absolute: Boolean = false

  def powerInt(x: Int, p: Int): Try[Int] =
    if (p >= 0) Rational.narrow(BigInt(x).pow(p), Int.MinValue, Int.MaxValue).map(_.toInt)
    else Failure(NumberException("negative power (Int)"))

}


/**
  * Trait for the query functions: three functions, corresponding to the functions for Int, Rational and Double representations.
  */
trait QueryFunctions[T] {
  val fInt: Int => Try[T]
  val fRat: Rational => Try[T]
  val fDouble: Double => Try[T]
}

/**
  * Definitions of QueryOperations.
  */
sealed trait QueryOperation[T] {
  def getFunctions: QueryFunctions[T]
}

case object QueryOperationIsZero extends QueryOperation[Boolean] {
  def getFunctions: QueryFunctions[Boolean] = new QueryFunctions[Boolean] {
    val fInt: Int => Try[Boolean] = tryF[Int, Boolean](x => x == 0)
    val fRat: Rational => Try[Boolean] = tryF[Rational, Boolean](x => x.signum == 0)
    val fDouble: Double => Try[Boolean] = tryF[Double, Boolean](x => x.sign == 0 || x.sign == -0)
  }
}

case object QueryOperationIsInfinite extends QueryOperation[Boolean] {
  def getFunctions: QueryFunctions[Boolean] = new QueryFunctions[Boolean] {
    val fInt: Int => Try[Boolean] = tryF[Int, Boolean](_ => false)
    val fRat: Rational => Try[Boolean] = tryF[Rational, Boolean](x => x.isInfinity)
    val fDouble: Double => Try[Boolean] = tryF[Double, Boolean](x => x == Double.PositiveInfinity || x == Double.NegativeInfinity)
  }
}

case object QueryOperationSignum extends QueryOperation[Int] {
  def getFunctions: QueryFunctions[Int] = new QueryFunctions[Int] {
    val fInt: Int => Try[Int] = tryF[Int, Int](math.signum)
    val fRat: Rational => Try[Int] = tryF[Rational, Int](_.signum)
    val fDouble: Double => Try[Int] = tryF[Double, Int](math.signum(_).toInt)
  }
}

object Operations {

  /**
    * Evaluate a dyadic operator on value and other, using the various functions passed in.
    *
    * @param value     the first operand, a Value.
    * @param other     the other operand, a Value.
    * @param functions the tuple of four conversion functions.
    * @return an optional Value which is result of applying the appropriate function to the operands value and other.
    */
  def doComposeValueDyadic(value: Value, other: Value)(functions: DyadicFunctions): Option[Value] = {
    val (fInt, fRational, fDouble) = functions

    def tryDouble(xo: Option[Double]): Try[Value] = xo match {
      case Some(n) => FP.sequence(for (y <- Value.maybeDouble(other)) yield fDouble(n, y)) map Value.fromDouble
      case None => Failure(NumberException("number is invalid")) // NOTE this case is not observed in practice
    }

    def tryConvert[X](x: X, msg: String)(extract: Value => Option[X], func: (X, X) => Try[X], g: X => Value): Try[Value] =
      toTryWithThrowable(for (y <- extract(other)) yield func(x, y) map g, NumberException(s"other is not a $msg")).flatten

    def tryRational(x: Rational): Try[Value] = tryConvert(x, "Rational")(v => Value.maybeRational(v), fRational, Value.fromRational)

    def tryInt(x: Int): Try[Value] = tryConvert(x, "Int")(v => Value.maybeInt(v), fInt, Value.fromInt)

    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Value] = y => tryMap(y)(tryRational, tryDouble)

    tryMap(value)(tryInt, xToZy1).toOption
  }

  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    * The result is an Option[Value] rather than a Number, as in Number's doComposeMonadic.
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

  def doQuery[T](v: Value, functions: QueryFunctions[T]): Option[T] = {
    val (fInt, fRational, fDouble) = (functions.fInt, functions.fRat, functions.fDouble) // TODO improve this
    val xToZy0: Option[Double] => Try[T] = {
      case Some(n) => fDouble(n)
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[T] = y => tryMap(y)(x => fRational(x), xToZy0)
    tryMap(v)(x => fInt(x), xToZy1).toOption
  }

}