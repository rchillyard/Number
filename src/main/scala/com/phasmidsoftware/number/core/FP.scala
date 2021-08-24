package com.phasmidsoftware.number.core

import java.util.NoSuchElementException
import scala.language.implicitConversions
import scala.util.{Either, Failure, Left, Right, Success, Try}

/**
  * This module is concerned with the generic operations for operating on Numbers.
  *
  */
object FP {
  /**
    * Sequence method to invert the order of types Option/Try.
    *
    * @param xyo an Option of Try[X].
    * @tparam X the underlying type.
    * @return a Try of Option[X].
    */
  def sequence[X](xyo: Option[Try[X]]): Try[Option[X]] = xyo match {
    case Some(Success(x)) => Success(Some(x))
    case Some(Failure(x)) => Failure(x)
    case None => Success(None)
  }

  /**
    * This method and tryMap are (almost) twins (working for Option and Try respectively --
    * although there are a few differences other than the monad type).
    * To yield the (optional) result, we map the right-hand member of the input (lRe) with a function rToZ.
    * We return either the result or, if empty, then the result of flatMap applied to the left member with a function lToZo.
    *
    * @param lRe  the input, an Either[L,R].
    * @param r2Z  a function R => Z.
    * @param l2Zo a function L => Option[Z].
    * @tparam L the type of the left-side of the Either.
    * @tparam R the type of the right-side of the Either.
    * @tparam Z the underlying type of the result.
    * @return an Option[Z]
    */
  def optionMap[L, R, Z](lRe: Either[L, R])(r2Z: R => Z, l2Zo: L => Option[Z]): Option[Z] =
    lRe.toOption.map(r2Z) orElse lRe.left.toOption.flatMap(l2Zo)

  /**
    * This method and optionMap are (almost) twins (working for Try and Option respectively).
    * To yield the (tried) result, we map the right-hand member of the input (lRe) with a function rToZ.
    * The result of this is then pattern-matched:
    * In the Some(z) case, we return the Success(z).
    * In the None case, we return the result of tryMapLeft applied to the lRe with the function l2Zy.
    *
    * NOTE: in practice, this does not seem to be used, but it should remain here.
    *
    * @param lRe  the input, an Either[L,R].
    * @param r2Z  a function R => Z.
    * @param l2Zy a function L => Try[Z]
    * @tparam L the type of the left-side of the Either.
    * @tparam R the type of the right-side of the Either.
    * @tparam Z the underlying type of the result.
    * @return a Try[Z]
    */
  def doMap[L, R, Z](lRe: Either[L, R])(r2Z: R => Z, l2Zy: L => Try[Z]): Try[Z] =
    lRe.toOption.map(r2Z) match {
      case Some(z) => Success(z)
      case None => tryMapLeft(lRe, l2Zy)
    }

  /**
    * This method is similar to doMap but with some important differences.
    * To yield the (tried) result, we map the right-hand member of the input (lRe) with a function rToZy.
    * The result of this is then pattern-matched:
    * In the Some(Success(z)) case, we return the Success(z).
    * In the Some(Failure) case, we invoke tryMapLeft with the transpose of lRe and the function l2Zy.
    * In the None case, we return the result of tryMapLeft applied to the lRe with the function l2Zy.
    *
    * @param lRe  the input, an Either[L,R].
    * @param r2Zy a function R => Try[Z].
    * @param l2Zy a function L => Try[Z]
    * @param r2L  an (implicit) converter from R to L.
    * @tparam L the type of the left-side of the Either.
    * @tparam R the type of the right-side of the Either.
    * @tparam Z the underlying type of the result.
    * @return a Try[Z]
    */
  def tryMap[L, R, Z](lRe: Either[L, R])(r2Zy: R => Try[Z], l2Zy: L => Try[Z])(implicit r2L: R => L): Try[Z] =
    lRe.toOption.map(r2Zy) match {
      case Some(Success(z)) => Success(z)
      case Some(Failure(_)) => tryMapLeft(transpose(lRe), l2Zy)
      case None => tryMapLeft(lRe, l2Zy)
    }

  /**
    * This method operates on the left member of xYe with xToZy and is invoked if tryMap fails.
    */

  /**
    * This method is invoked by tryMap when the input is "left" or when the r2Zy method fails.
    * To yield the (tried) result, we map the left-hand member of the input (lRe) with a function l2Zy.
    * The wrapped value of this is then returned (unless empty, in which case a Failure is returned).
    *
    * @param lRe  the input, an Either[L,R].
    * @param l2Zy a function L => Try[Z]
    * @tparam L the type of the left-side of the Either.
    * @tparam R the type of the right-side of the Either.
    * @tparam Z the underlying type of the result.
    * @return a Try[Z]
    */
  private def tryMapLeft[L, R, Z](lRe: Either[L, R], l2Zy: L => Try[Z]): Try[Z] =
    lRe.left.toOption.map(l2Zy) getOrElse Failure(new NoSuchElementException)

  /**
    * If lRe is a Right(R), then we transpose it into a Left(L).
    *
    * @param lRe an Either[L, R].
    * @tparam L the left type.
    * @tparam R the right type.
    * @return a Left(L) as an Either[L, R].
    */
  def transpose[L, R](lRe: Either[L, R])(implicit rToL: R => L): Either[L, R] = lRe match {
    case Right(y) => Left(rToL(y))
    case Left(_) => lRe
  }

  /**
    * This method fills a gap in the Scala library.
    * It converts an Option[X] to a Try[X] with the benefit of a default value.
    *
    * @param xo      an Option[X].
    * @param default the value of Try[X] to be returned in the event that xo is empty.
    * @tparam X the underlying type of input and output.
    * @return a Try[X]
    */
  def toTry[X](xo: Option[X], default: => Try[X]): Try[X] = xo match {
    case Some(x) => Success(x)
    case None => default
  }

  /**
    * This method fills a gap in the Scala library.
    * It converts an Option[X] to a Try[X] with the benefit of a default value.
    *
    * @param xo      an Option[X].
    * @param default a Throwable to be returned as a Failure in the event that xo is empty.
    * @tparam X the underlying type of input and output.
    * @return a Try[X]
    */
  def toTryWithThrowable[X](xo: Option[X], default: => Throwable): Try[X] = toTry(xo, Failure(default))

  /**
    * This method is a substitute for Try.apply in the case that we want it as a function
    * (otherwise, we run into a type inference problem).
    *
    * @param x an X.
    * @tparam X the type of x.
    * @return Success(x)
    */
  def identityTry[X](x: X): Try[X] = Success(x)

  /**
    * Method to yield a Failure.
    *
    * @param e a Throwable
    * @tparam Z the underlying type of the result.
    * @return a function of Z => Failure[Z] based on e
    */
  def fail[X, Z](e: Throwable): X => Try[Z] = _ => Failure(e)

  /**
    * Method to yield a Failure.
    *
    * @param s a String
    * @tparam Z the underlying type of the result.
    * @return a function of Z => Failure[Z] based on NumberException(s)
    */
  def fail[X, Z](s: String): X => Try[Z] = fail(NumberException(s))

  /**
    * Method to yield a partially lifted version of a Function1 as a function.
    *
    * @param f an X => Z.
    * @tparam X the type of the input to f and the input to the resulting function.
    * @tparam Z the type of the output from f and the underlying type of the resulting function.
    * @return a function X => Try[Z]
    */
  def tryF[X, Z](f: X => Z): X => Try[Z] = x => Try(f(x))

  /**
    * Method to yield a partially lifted version of a Function2 as a function.
    *
    * @param f an (X,Y) => Z.
    * @tparam X the type of the first input to f and of the resulting function.
    * @tparam Y the type of the second input to f and of the resulting function.
    * @tparam Z the type of the output from f and the underlying type of the resulting function.
    * @return a function X => Try[Z]
    */
  def tryF[X, Y, Z](f: (X, Y) => Z): (X, Y) => Try[Z] = (x, y) => Try(f(x, y))
}

/**
  * These converters are used by the tryMap and transpose.
  */
object Converters {
  implicit def convertIntToRational(x: Int): Either[Option[Double], Rational] = Right(Rational(x))

  implicit def convertRationalToOptionalDouble(x: Rational): Option[Double] = Try(x.toDouble).toOption
}
