package com.phasmidsoftware.number.misc

import java.util.NoSuchElementException

import com.phasmidsoftware.number.core.Rational

import scala.language.implicitConversions
import scala.math.BigInt
import scala.util.{Either, Failure, Left, Right, Success, Try}

/**
  * This module is concerned with the generic operations for operating on Numbers.
  *
  */
object FP {

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
  * These converters are used by the tryMap and tryAlt
  */
object Converters {
  implicit def convertIntToBigInt(x: Int): Either[Either[Option[Double], Rational], BigInt] = Right(BigInt(x))

  implicit def convertBigIntToRational(x: BigInt): Either[Option[Double], Rational] = Right(Rational(x))

  implicit def convertRationalToOptionalDouble(x: Rational): Option[Double] = Try(x.toDouble).toOption
}
