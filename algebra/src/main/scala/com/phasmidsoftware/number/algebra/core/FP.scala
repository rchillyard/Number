/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.core.inner.{Rational, RationalException}
import java.net.URL
import scala.Option.when
import scala.io.Source
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Using.Releasable
import scala.util.{Either, Failure, Left, Right, Success, Try, Using}

/**
  * This module is concerned with the generic operations for operating on Numbers.
  * TODO move this into the core module.
  */
object FP {

  /**
    * Executes the provided computation if the given condition is true; otherwise, returns a Failure.
    *
    * @param p A Boolean condition that determines whether the computation should be executed.
    * @param x A computation represented as a call-by-name parameter, which returns a Try[X] when evaluated.
    * @return A Try[X] representing the result of the computation if the condition is true, or a Failure if the condition is false.
    */
  def whenTry[X](p: Boolean)(x: => X): Try[X] =
    if p then Success(x) else Failure(new Exception(s"condition $p is not satisfied"))

  /**
    * Executes the provided computation if the given condition is true; otherwise, returns a Failure.
    *
    * @param p A Boolean condition that determines whether the computation should be executed.
    * @param x A computation represented as a call-by-name parameter, which returns a Try[X] when evaluated.
    * @return A Try[X] representing the result of the computation if the condition is true, or a Failure if the condition is false.
    */
  def wheneverTry[X](p: Boolean)(x: => Try[X]): Try[X] =
    whenTry(p)(x).flatten

  /**
    * Executes the provided block of code conditionally based on the given boolean predicate.
    * If the predicate is true, the block of code is executed and its result is returned.
    * If the predicate is false, None is returned.
    *
    * @param p the boolean predicate that determines whether the block of code should be executed
    * @param x a by-name parameter representing the block of code returning an Option of type X
    * @return an Option containing the result of the block of code if `p` is true, or None if `p` is false
    */
  def whenever[X](p: Boolean)(x: => Option[X]): Option[X] =
    when(p)(x).flatten

  /**
    * Sequence method to combine elements of Try.
    *
    * @param xys an Iterator of Try[X]
    * @tparam X the underlying type
    * @return a Try of Iterator[X]
    */
  def sequence[X](xys: Iterator[Try[X]]): Try[Iterator[X]] =
    sequence(xys.to(List)).map(_.iterator)

  /**
    * Sequence method to combine elements of Try.
    *
    * @param xys an Iterable of Try[X]
    * @tparam X the underlying type
    * @return a Try of Seq[X]
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xys: Iterable[Try[X]]): Try[Seq[X]] =
    xys.foldLeft(Try(Seq[X]())) {
      (xsy, xy) => for xs <- xsy; x <- xy yield xs :+ x
    }

  /**
    * Sequence method to combine elements of Try.
    *
    * @param xos an Iterator of Try[X]
    * @tparam X the underlying type
    * @return a Try of Iterator[X]
    */
  def sequence[X](xos: Iterator[Option[X]]): Option[Iterator[X]] =
    sequence(xos.to(List)).map(_.iterator)

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
    * Method to convert a None value to a given exception (rather than the NoSuchElement exception).
    *
    * @param to an Option[T].
    * @param x  a Throwable to be thrown if to is None.
    * @tparam T the underlying type of to.
    * @return t if to is Some(t); otherwise x will be thrown.
    */
  def recover[T](to: Option[T])(x: => Throwable): T = to match {
    case Some(t) => t
    case None => throw x
  }

  def recoverAsTry[T](to: Option[T])(x: => Throwable): Try[T] = to match {
    case Some(t) => Success(t)
    case None => Failure(x)
  }

  def recoverWithTry[T](to: Option[T])(x: => Try[T]): Try[T] = to match {
    case Some(t) => Success(t)
    case None => x
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
    * TESTME (unused)
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
    * This method is similar to `doMap` but with some important differences.
    * To yield the (tried) result, we map the right-hand member of the input (`lRe`) with a function `rToZy`.
    * The result of this is then pattern-matched:
    * In the case of `Some(Success(z))`, we return the `Success(z)`.
    * In the case of `Some(Failure)`, we invoke `tryMapLeft` with the transpose of `lRe` and the function `l2Zy`.
    * In the case of `None`, we return the result of `tryMapLeft` applied to the `lRe` with the function `l2Zy`.
    *
    * @param lRe  the input, an `Either[L,R]`.
    * @param r2Zy a function `R => Try[Z]`.
    * @param l2Zy a function `L => Try[Z]`.
    * @param r2L  an (implicit) converter from `R` to `L`.
    * @tparam L the type of the left-side of the `Either`.
    * @tparam R the type of the right-side of the `Either`.
    * @tparam Z the underlying type of the result.
    * @return a `Try[Z]`
    */
  def tryMap[L, R, Z](lRe: Either[L, R])(r2Zy: R => Try[Z], l2Zy: L => Try[Z])(using r2L: R => L): Try[Z] =
    lRe.toOption.map(r2Zy) match {
      case Some(Success(z)) => Success(z)
      case Some(Failure(_)) => tryMapLeft(transpose(lRe), l2Zy)
      case None => tryMapLeft(lRe, l2Zy)
    }

  /**
    * If lRe is a Right(R), then we transpose it into a Left(L).
    *
    * @param lRe an Either[L, R].
    * @tparam L the left type.
    * @tparam R the right type.
    * @return a Left(L) as an Either[L, R].
    */
  private def transpose[L, R](lRe: Either[L, R])(using rToL: R => L): Either[L, R] = lRe match {
    case Right(y) => Left(rToL(y))
    case Left(_) => lRe
  }

  /**
    * Method to convert an `Option` into a `Try` where the default is a `String`.
    *
    * @param xo      an `Option[X]`.
    * @param default a `Throwable`.
    * @tparam X the underlying type of both input and output.
    * @return if `xo` is `Some(x)` then `Success(x)` else `Failure(RationalException(default))`.
    */
  def toTryWithRationalException[X](xo: Option[X], default: => String): Try[X] =
    toTryWithThrowable(xo, RationalException(default))

  /**
    * Method to convert an `Option` into a `Try` where the default is an exception, i.e., a `Throwable`.
    *
    * @param xo      an `Option[X]`.
    * @param default a `Throwable`.
    * @tparam X the underlying type of both input and output.
    * @return if `xo` is `Some(x)` then `Success(x)` else `Failure(default)`.
    */
  def toTryWithThrowable[X](xo: Option[X], default: => Throwable): Try[X] =
    xo.toRight(default).toTry // toTry(xo, Failure(default))

  /**
    * This method is a substitute for `Try.apply` in the case that we want it as a function
    * (otherwise, we run into a type inference problem).
    *
    * @param x an X.
    * @tparam X the type of x.
    * @return Success(x)
    */
  def identityTry[X](x: X): Try[X] = Success(x)

  /**
    * Terminates the computation by producing a failure encapsulated in a Try instance.
    *
    * @param s The error message to be included in the exception.
    * @return A Try instance containing a failure with an AlgebraException
    *         initialized with the provided error message.
    */
  def fail[Z](s: String): Try[Z] =
    fail(AlgebraException(s))

  /**
    * Creates a failed instance of `Try` containing the provided exception.
    *
    * @param e The `Throwable` representing the failure reason.
    * @return A `Failure` wrapping the given exception.
    */
  def fail[Z](e: Throwable): Try[Z] =
    Failure(e)

  /**
    * Method to yield a partially lifted version of a Function1 as a function.
    *
    * @param f an X => Z.
    * @tparam X the type of the input to f and the input to the resulting function.
    * @tparam Z the type of the output from f and the underlying type of the resulting function.
    * @return a function X => Try[Z]
    */
  def tryF[X, Z](f: X => Z): X => Try[Z] =
    x => Try(f(x))

  /**
    * Method to yield a partially lifted version of a Function2 as a function.
    *
    * @param f an (X,Y) => Z.
    * @tparam X the type of the first input to f and of the resulting function.
    * @tparam Y the type of the second input to f and of the resulting function.
    * @tparam Z the type of the output from f and the underlying type of the resulting function.
    * @return a function X => Try[Z]
    */
  def tryF[X, Y, Z](f: (X, Y) => Z): (X, Y) => Try[Z] =
    (x, y) => Try(f(x, y))

  /**
    * Method to yield an Option of T according to whether the predicate p yields true.
    *
    * @param p a predicate on T.
    * @param t an actual value of T.
    * @tparam T the type of t (and the underlying type of the result).
    * @return Some(t) if p(t) is true, otherwise None.
    */
  def optional[T](p: T => Boolean)(t: T): Option[T] =
    Some(t).filter(p)

  /**
    * Method to get the value of an Option[X] but throwing a given exception rather than the usual NoSuchElement.
    *
    * @param xo an optional value of X (called by name).
    * @param t  a throwable.
    * @tparam X the underlying type of xo and the type of the result.
    * @return the value of xo or throws t.
    * @throws java.lang.Throwable t
    */
  def getOrThrow[X](xo: => Option[X], t: => Throwable): X =
    xo.getOrElse(throw t)

  /**
    * Reads data from a specified resource file, applies a transformation function to each line,
    * and converts the resulting values into a sequence of BigInt objects.
    *
    * @param filename the name of the resource file to read from
    * @param function a function that takes an array of strings (split from a line)
    *                 and returns an optional string result after processing
    *
    * @return a `Try` containing a sequence of BigInt values if successful, or a failure if
    *         an error occurs during processing or if the file contains invalid input
    */
  def readFromResource(filename: String, function: Array[String] => Option[String]): Try[Seq[BigInt]] =
    TryUsing(FP.resource[FP.type](filename).map(Source.fromURL(_))) {
      source =>
        val bn = implicitly[math.Numeric[BigInt]]
        val wos: Iterator[Option[String]] = source.getLines().map(l => function(l.split("""\s""")))
        val bos: Iterator[Option[BigInt]] = for p <- wos yield for q <- p; qq <- bn.parseString(q) yield qq
        FP.toTry(FP.sequence(bos.toList), Failure(AlgebraException(s"invalid input in file: $filename")))
    }

  /**
    * Sequence method to combine elements of type Option[X].
    *
    * @param xos an Iterable of Option[X].
    * @tparam X the underlying type.
    * @return if <code>xos</code> contains any Nones, the result will be None, otherwise Some(...).
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xos: Iterable[Option[X]]): Option[Seq[X]] =
    xos.foldLeft(Option(Seq[X]())) {
      (xso, xo) => for xs <- xso; x <- xo yield xs :+ x
    }

  /**
    * Method to yield a URL for a given resourceForClass in the classpath for C.
    *
    * @param resourceName the name of the resourceForClass.
    * @tparam C a class of the package containing the resourceForClass.
    * @return a Try[URL].
    */
  def resource[C: ClassTag](resourceName: String): Try[URL] =
    resourceForClass(resourceName, implicitly[ClassTag[C]].runtimeClass)

  /**
    * Method to yield a Try[URL] for a resource name and a given class.
    *
    * @param resourceName the name of the resource.
    * @param clazz        the class, relative to which, the resource can be found (defaults to the caller's class).
    * @return a Try[URL]
    */
  def resourceForClass(resourceName: String, clazz: Class[?] = getClass): Try[URL] =
    Option(clazz.getResource(resourceName)) match {
      case Some(u) => Success(u)
      case None => Failure(AlgebraException(s"$resourceName is not a valid resource for $clazz"))
    }

  /**
    * Method to convert an `Option` into a `Try`.
    *
    * @param xo      an `Option[X]`.
    * @param default a `Try[X]`.
    * @tparam X the underlying type of both input and output.
    * @return if `xo` is `Some(x)` then `Success(x)` else `default`.
    */
  def toTry[X](xo: Option[X], default: => Try[X]): Try[X] =
    xo map (Success(_)) getOrElse default

  /**
    * Converts a `Try` instance into an `Option`.
    * Any `Exception` in the input is of course lost.
    *
    * @param xy The `Try` instance to be converted.
    * @return An `Option` containing the value if the `Try` is a Success, or `None` if the `Try` is a `Failure`.
    */
  def toOption[X](xy: Try[X]): Option[X] =
    toOptionWithLog(t => System.err.println(s"FP.toOption: $t"))(xy)

  def toOptionWithLog[X](log: Throwable => Unit)(xy: Try[X]): Option[X] = xy match {
    case Success(x) => Some(x)
    case Failure(x) =>
      log(x)
      None
  }


  /**
    * Computes the power of a given number by raising it to the specified exponent.
    *
    * CONSIDER moving this somewhere else more aligned to Numeric ops
    *
    * @param x the base number
    * @param n the exponent to which the base number is raised
    * @return the result of raising the base number to the specified exponent
    */
  def power[X: math.Numeric](x: X, n: Int): X =
    if n == 0 then
      implicitly[math.Numeric[X]].one
    else
      implicitly[math.Numeric[X]].times(x, power(x, n - 1))

}

/**
  * These converters are used by the tryMap and transpose.
  */
object Converters {
  implicit def convertIntToRational(x: Int): Either[Option[Double], Rational] =
    Right(Rational(x))

  implicit def convertRationalToOptionalDouble(x: Rational): Option[Double] =
    Try(x.toDouble).toOption
}


object TryUsing {
  /**
    * This method is similar to `apply(r)` but it takes a `Try[R]` as its parameter.
    * The definition of f is the same as in the other apply, however.
    *
    * @param ry a Try[R] which is passed into f and will be managed via Using.apply
    * @param f  a function of R => Try[A].
    * @tparam R the resource type.
    * @tparam A the underlying type of the result.
    * @return a Try[A]
    */
  def apply[R: Releasable, A](ry: Try[R])(f: R => Try[A]): Try[A] =
    for r <- ry; a <- apply(r)(f) yield a

  /**
    * This method is to `Using.apply` as `flatMap` is to `map`.
    *
    * @param resource a resource which is used by f and will be managed via Using.apply
    * @param f        a function of R => Try[A].
    * @tparam R the resource type.
    * @tparam A the underlying type of the result.
    * @return a Try[A]
    */
  def apply[R: Releasable, A](resource: => R)(f: R => Try[A]): Try[A] =
    Using(resource)(f).flatten
}
