package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.{fail, toTryWithThrowable, tryF, tryMap}
import scala.annotation.tailrec
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
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * The general formula for a monadic operation is: x * dfByDx(x) / f(x)
    */
  val relativeFuzz: Double => Double

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
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

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to invert a Number.
  */
case object MonadicOperationInvert extends MonadicOperation {
  private def invertInt(x: Int): Try[Int] = x match {
    case 1 => Success(1)
    case _ => Failure(NumberException("can't invert Int"))
  }

  private val xf: Fractional[Double] = implicitly[Fractional[Double]]

  private def invertDouble(x: Double): Try[Double] = Try(xf.div(xf.one, x))

  val functions: MonadicFunctions = (invertInt, tryF[Rational, Rational](x => x.invert), invertDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * For any power that is not 0, the result is simply the power.
    */
  val relativeFuzz: Double => Double = _ => -1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to raise e (Euler's number) to the power of a Number.
  */
case object MonadicOperationExp extends MonadicOperation {
  private def expInt(x: Int): Try[Int] = x match {
    case 0 => Success(1)
    case _ => Failure(NumberException("can't exp Int"))
  }

  private val expRat: Rational => Try[Rational] = {
    case r if r.isInfinity && r.signum < 0 => Success(Rational.zero)
    case r => fail("can't do exp Rational=>Rational for non-zero parameter")(r)
  }

  private def expDouble(x: Double): Try[Double] = Try(Math.exp(x))

  val functions: MonadicFunctions = (
      expInt,
      expRat,
      expDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => x

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to yield the natural logarithm of a Number.
  */
case object MonadicOperationLog extends MonadicOperation {

  private def logInt(x: Int): Try[Int] = x match {
    case 1 => Success(0)
    case _ => Failure(NumberException("can't log Int"))
  }

  private val logRat: Rational => Try[Rational] = {
    case Rational.zero => Success(Rational.infinity.negate)
    case r => fail("can't do log Rational=>Rational for parameter")(r)
  }

  private def logDouble(x: Double): Try[Double] = Try(Math.log(x))

  val functions: MonadicFunctions = (
      logInt,
      logRat,
      logDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => 1 / math.log(x) // the reciprocal of the natural log of x

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to calculate the sine of a Number.
  * The number is a factor of pi, i.e. it is in radians.
  *
  * NOTE that this evaluator for sine is not as useful as the evaluator in the Number class.
  * That's because this type of operation does not change the factor.
  *
  * CONSIDER allow Monadic operations to change factor.
  *
  * See https://en.wikipedia.org/wiki/List_of_trigonometric_identities
  */
case object MonadicOperationSin extends MonadicOperation {
  // XXX any integral angle (in radians) results in a zero sine value.
  private val sinInt: Int => Try[Int] = tryF(_ => 0)

  private val (two, six) = (BigInt(2), BigInt(6))

  /**
    * NOTE that we declare this as a def so that we can invoke it for negative values.
    * Also NOTE that the modulo 4 and modulo 12 checks aren't necessary if the value has been modulated to -1 to 1.
    *
    * @param x a Rational.
    * @return a Try[Rational].
    */
  private def sinRatExact(x: Rational): Try[Rational] =
    if (x.isNegative) sinRatExact(x.negate).map(_.negate)
    else (x.n, x.d) match {
      case (n, `two`) if n.isValidInt => n.toInt match {
        case t if t % 4 == 1 => Success(Rational.one)
        case t if t % 4 == 3 => Success(Rational.one.negate)
        case _ => Failure(NumberException("sine cannot be exact Rational"))
      }
      case (n, `six`) if n.isValidInt => n.toInt match {
        case t if t % 12 == 1 || t % 12 == 5 => Success(Rational.half)
        case t if t % 12 == 7 || t % 12 == 11 => Success(Rational.half.negate)
        case _ => Failure(NumberException("sine cannot be exact Rational"))
      }
      case _ => Failure(NumberException("sine cannot be exact Rational"))
    }

  private val sinRatInexact: Rational => Try[Rational] = x =>
    if (!x.invert.isWhole) sinDouble(x.toDouble).map(Rational(_))
    else Failure(NumberException("MonadicOperationSin: logic error: whole Rational"))

  private def sinDouble(x: Double): Try[Double] = Try(Math.sin(x * math.Pi))

  val functions: MonadicFunctions = (sinInt, r => sinRatExact(r) orElse sinRatInexact(r), sinDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => x / math.tan(x)

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3
}

/**
  * MonadicOperation to yield the arctangent of a Number.
  *
  * @param sign an Int which will distinguish between results in the four quadrants.
  */
case class MonadicOperationAtan(sign: Int) extends MonadicOperation {

  private val atanRat: Rational => Try[Rational] = r => modulateAngle(doAtan(r.abs), r.signum < 0)

  def atan(x: Double): Try[Double] =
    Try {
      math.atan2(x, sign) / math.Pi
    } // CONSIDER use scale // TESTME

  val functions: MonadicFunctions = (fail("atan cannot be Int"), atanRat, atan)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => x / (1 + math.pow(x, 2)) / math.atan2(x, sign)

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 4

  def modulateAngle(ry: Try[Rational], flip: Boolean): Try[Rational] = ry map (r => if (flip) r.negate else r) map (r => if (sign < 0) Rational.one + r else r)

  private def doAtan(r: Rational) = {
    r match {
      case Rational.infinity => Success(Rational.half)
      case Rational.zero => Success(Rational.zero)
      case Rational.one => Success(Rational.one / 4)
      case _ => Failure(NumberException("atan cannot be Rational"))
    }
  }
}

/**
  * MonadicOperation to yield the modulus a Number.
  *
  * CONSIDER this description is not very helpful. Is it really the modulus?
  */
case class MonadicOperationModulate(min: Int, max: Int, circular: Boolean) extends MonadicOperation {
  private def modulate[X: Numeric](z: X, min: X, max: X): X = {
    import scala.math.Numeric.Implicits.infixNumericOps

    @tailrec
    def inner(result: X): X =
      if (result < min) inner(result + max - min)
      else if (result > max) inner(result + min - max)
      else if (circular && result == min) max
      else result

    inner(z)
  }

  val functions: MonadicFunctions = (
      tryF(z => modulate(z, min, max)),
      tryF(z => modulate(z, Rational(min), Rational(max))),
      tryF(z => modulate(z, min, max))
  )

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to yield the square root of a Number.
  *
  * CONSIDER eliminating this and using power only.
  */
case object MonadicOperationSqrt extends MonadicOperation {
  private val sqrtInt: Int => Try[Int] = // CONSIDER not using squareRoots: there are other ways.
    x => toTryWithThrowable(Rational.squareRoots.get(x), NumberException("Cannot create Int from Double"))

  private val sqrtRat: Rational => Try[Rational] = x => FP.toTry(x.root(2), Failure(NumberException("Cannot get exact square root")))

  val functions: MonadicFunctions = (sqrtInt, sqrtRat, tryF(x => math.sqrt(x)))

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * For any power, the result is simply the power.
    */
  val relativeFuzz: Double => Double = _ => 0.5

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
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

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to apply a generic function to a Number.
  *
  */
case class MonadicOperationFunc(f: Double => Double, dfByDx: Double => Double) extends MonadicOperation {
  val functions: MonadicFunctions = (fail("no apply"), fail("no apply"), tryF(f))

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * This is the general formula for a monadic operation. All others derive from this formula.
    */
  val relativeFuzz: Double => Double = x => x * dfByDx(x) / f(x)

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

  private def powerInt(x: Int, p: Int): Try[Int] =
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
    val (fInt, fRational, fDouble) = (functions.fInt, functions.fRat, functions.fDouble) // CONSIDER improve this (what's the problem?)
    val xToZy0: Option[Double] => Try[T] = {
      case Some(n) => fDouble(n)
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[T] = y => tryMap(y)(x => fRational(x), xToZy0)
    tryMap(v)(x => fInt(x), xToZy1).toOption
  }

}