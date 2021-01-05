package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.forceIntoRange
import com.phasmidsoftware.number.core.Operations.{DyadicFunctions, MonadicFunctions}
import com.phasmidsoftware.number.misc.FP.{toTry, tryF}

import scala.language.implicitConversions
import scala.math.BigInt
import scala.util._

/**
  * This module is concerned with the generic operations for operating on Numbers.
  *
  * Most of the code is generic.
  * Some of the code is specific to particular operations.
  */
object Operations {

  type DyadicFunctions = ((Int, Int) => Try[Int], (BigInt, BigInt) => Try[BigInt], (Rational, Rational) => Try[Rational], (Double, Double) => Try[Double])

  type MonadicFunctions = (Int => Try[Int], BigInt => Try[BigInt], Rational => Try[Rational], Double => Try[Double])

  //  type MonadicTransformations = (Int => Try[Double], BigInt => Try[Double], Rational => Try[Double], Double => Try[Double])

  type QueryFunctions = (Int => Try[Boolean], BigInt => Try[Boolean], Rational => Try[Boolean], Double => Try[Boolean])
}

/**
  * Definitions of MonadicOperations.
  */
sealed trait MonadicOperation {
  def getFunctions: MonadicFunctions
}

case object MonadicOperationNegate extends MonadicOperation {
  def getFunctions: MonadicFunctions = {
    val fInt = tryF[Int, Int](math.negateExact)
    val fBigInt = tryF[BigInt, BigInt](implicitly[Numeric[BigInt]].negate)
    val fRational = tryF[Rational, Rational](implicitly[Numeric[Rational]].negate)
    val fDouble = tryF[Double, Double](implicitly[Numeric[Double]].negate)
    (fInt, fBigInt, fRational, fDouble)
  }
}

case object MonadicOperationSin extends MonadicOperation {
  def sinDouble(x: Double): Try[Double] = Try(Math.sin(x * math.Pi))

  val sinRat: Rational => Try[Rational] = x =>
    if (!x.invert.isWhole) sinDouble(x.toDouble).map(Rational(_))
    else x.invert.toInt match {
      case 6 => Success(Rational.half)
      case 4 => Success(Rational.half.sqrt)
      case 3 => Success(Rational(3).sqrt / 2)
      case 2 => Success(Rational.one)
      case _ => Failure(NumberException("sine cannot be Rational"))
    }

  def getFunctions: MonadicFunctions = (tryF[Int, Int](_ => 0), tryF[BigInt, BigInt](_ => 0), sinRat, sinDouble)
}

case class MonadicOperationAtan(sign: Int) extends MonadicOperation {
  def atan(x: Double): Try[Double] =
    Try(math.atan2(x, sign) / math.Pi) // TODO use scale

  val atanRat: Rational => Try[Rational] = x =>
    if (!x.isWhole) atan(x.toDouble).map(Rational(_))
    else x.toInt match {
      case -1 => Success(Rational.one * 3 / 4 + (if (sign < 0) Rational.one else Rational.zero))
      case 0 => Success(Rational.zero + (if (sign < 0) Rational.one else Rational.zero))
      case 1 => Success(Rational.one / 4 + (if (sign < 0) Rational.one else Rational.zero))
      case _ => Failure(NumberException("atan cannot be Rational"))
    }

  def getFunctions: MonadicFunctions = (_ =>
    Failure(NumberException("atan cannot be Rational")), _ =>
    Failure(NumberException("atan cannot be Rational")), atanRat, atan)
}

case object MonadicOperationModulate extends MonadicOperation {
  def getFunctions: MonadicFunctions = (
    tryF(z => math.floorMod(z, 2)),
    _ => Failure(NumberException("Can't modulate on BigInt")),
    _ => Failure(NumberException("Can't modulate on Rational")),
    tryF(z => forceIntoRange(z, 0, 2)))
}

case object MonadicOperationSqrt extends MonadicOperation {
  val sqrtInt: Int => Try[Int] = x => toTry(Rational.squareRoots.get(x), Failure[Int](NumberException("Cannot create Int from Double")))

  def getFunctions: MonadicFunctions =
    (sqrtInt, _ => Failure(NumberException("Can't sqrt on BigInt")), tryF(x => Rational.sqrt(x)), tryF(x => math.sqrt(x)))
}

case class MonadicOperationScale(f: Int) extends MonadicOperation {
  def getFunctions: MonadicFunctions = {
    val fInt = tryF[Int, Int](math.multiplyExact(_, f))
    val fBigInt = tryF[BigInt, BigInt](_ * f)
    val fRational = tryF[Rational, Rational](_ * f)
    val fDouble = tryF[Double, Double](_ * f)
    (fInt, fBigInt, fRational, fDouble)
  }
}

sealed trait DyadicOperation {
  def getFunctions: DyadicFunctions
}

case object DyadicOperationPlus extends DyadicOperation {
  def getFunctions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.addExact)
    val fBigInt = tryF[BigInt, BigInt, BigInt](implicitly[Numeric[BigInt]].plus)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].plus)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].plus)
    (fInt, fBigInt, fRational, fDouble)
  }
}

case object DyadicOperationTimes extends DyadicOperation {
  def getFunctions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.multiplyExact)
    val fBigInt = tryF[BigInt, BigInt, BigInt](implicitly[Numeric[BigInt]].times)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].times)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].times)
    (fInt, fBigInt, fRational, fDouble)
  }
}
