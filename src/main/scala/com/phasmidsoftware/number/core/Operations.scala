package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Operations.{DyadicFunctions, MonadicFunctions, toTry, tryF}
import java.util.NoSuchElementException
import scala.language.implicitConversions
import scala.math.BigInt
import scala.util._

object Operations {

  type DyadicFunctions = ((Int, Int) => Try[Int], (BigInt, BigInt) => Try[BigInt], (Rational, Rational) => Try[Rational], (Double, Double) => Try[Double])

  type MonadicFunctions = (Int => Try[Int], BigInt => Try[BigInt], Rational => Try[Rational], Double => Try[Double])

  //  type MonadicTransformations = (Int => Try[Double], BigInt => Try[Double], Rational => Try[Double], Double => Try[Double])

  type QueryFunctions = (Int => Try[Boolean], BigInt => Try[Boolean], Rational => Try[Boolean], Double => Try[Boolean])

  //noinspection ScalaUnusedSymbol
  private def optionToEither[X, Y](x: Option[X], y: => Y): Either[Y, X] = x.map(Right(_)).getOrElse(Left(y))

  def optionMap[X, Y, Z](xYe: Either[X, Y])(yToZ: Y => Z, xToZy: X => Option[Z]): Option[Z] = xYe.toOption.map(yToZ) match {
    case Some(z) => Some(z)
    case None => xYe.left.toOption.flatMap(xToZy)
  }

  /**
    * If xYe is a Right[Y], then we transpose it into a Left[X].
    *
    * @param xYe an Either[X, Y].
    * @tparam X the left type.
    * @tparam Y the right type.
    * @return a Left[X] as an Either[X, Y].
    */
  def transpose[X, Y](xYe: Either[X, Y])(implicit yToX: Y => X): Either[X, Y] = xYe match {
    case Right(y) => Left(yToX(y))
    case Left(_) => xYe
  }

  /**
    * This method operates on the right member of xYe with yToZy
    * If the right member doesn't exist or we are unsuccessful, we try the left member with xToZy.
    */
  def tryMap[X, Y, Z](xYe: Either[X, Y])(yToZy: Y => Try[Z], xToZy: X => Try[Z])(implicit yToX: Y => X): Try[Z] =
    xYe.toOption.map(yToZy) match {
      case Some(Success(z)) => Success(z)
      case Some(Failure(_)) => tryMapLeft(transpose(xYe), xToZy)
      case None => tryMapLeft(xYe, xToZy)
    }

  /**
    * This method operates on the left member of xYe with xToZy and is invoked if tryMap fails.
    */
  private def tryMapLeft[X, Y, Z](xYe: Either[X, Y], xToZy: X => Try[Z]): Try[Z] =
    xYe.left.toOption.map(xToZy) match {
      case Some(z) => z
      case None => Failure(new NoSuchElementException)
    }

  def toTry[X](xo: Option[X], default: Try[X]): Try[X] = xo match {
    case Some(x) => Success(x)
    case None => default
  }

  def identityTry[X](x: X): Try[X] = Success(x)

  def tryF[X, Y](f: X => Y): X => Try[Y] = x => Try(f(x))

  def tryF[X, Y, Z](f: (X, Y) => Z): (X, Y) => Try[Z] = (x, y) => Try(f(x, y))

}

/**
  * Definitions of MonadicOperations.
  * CONSIDER splitting these operation classes into a separate module.
  */
sealed trait MonadicOperation {
  def getFunctions: MonadicFunctions
}

case object MonadicOperationNegate extends MonadicOperation {
  def getFunctions: MonadicFunctions = {
    val fInt = tryF[Int, Int](implicitly[Numeric[Int]].negate)
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

case object MonadicOperationSqrt extends MonadicOperation {
  val sqrtInt: Int => Try[Int] = x => toTry(Rational.squareRoots.get(x), Failure[Int](NumberException("Cannot create Int from Double")))

  def getFunctions: MonadicFunctions =
    (sqrtInt, _ => Failure(NumberException("Can't sqrt on BigInt")), tryF(x => Rational.sqrt(x)), tryF(x => math.sqrt(x)))
}

case class MonadicOperationScale(f: Int) extends MonadicOperation {
  def getFunctions: MonadicFunctions = {
    val fInt = tryF[Int, Int](_ * f)
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
    val fInt = tryF[Int, Int, Int](implicitly[Numeric[Int]].plus)
    val fBigInt = tryF[BigInt, BigInt, BigInt](implicitly[Numeric[BigInt]].plus)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].plus)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].plus)
    (fInt, fBigInt, fRational, fDouble)
  }
}

case object DyadicOperationTimes extends DyadicOperation {
  def getFunctions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](implicitly[Numeric[Int]].times)
    val fBigInt = tryF[BigInt, BigInt, BigInt](implicitly[Numeric[BigInt]].times)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].times)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].times)
    (fInt, fBigInt, fRational, fDouble)
  }
}
