/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.matchers

import com.phasmidsoftware.matchers.{MatcherException, Matchers, ~}

/**
  * This trait extends a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  *
  * CONSIDER why to we need this trait?
  */
trait MatchersExtras extends Matchers {


  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P but which invokes m (which takes a 2-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function (T0, T1) => P. This is required only by the compiler, not used at run-time.
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from2[T0, T1, P <: Product, R](m: Matcher[T0 ~ T1, R])(f: (T0, T1) => P): Matcher[P, R] = p =>
    m(p.productElement(0).asInstanceOf[T0] ~ p.productElement(1).asInstanceOf[T1])

  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P but which invokes m (which takes a 2-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function P => Option[(T0, T1)] (in other words, an unapply method).
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from2Alt[T0, T1, P <: Product, R](m: Matcher[(T0, T1), R])(f: P => Option[(T0, T1)]): Matcher[P, R] = p =>
    f(p) match {
      case Some((t0, t1)) => m(t0, t1)
      case _ => Error(MatcherException("logic error"))
    }

  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P
    * but which invokes m (which takes a 3-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function (T0, T1) => P. This is required only by the compiler, not used at run-time.
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from3[T0, T1, T2, P <: Product, R](m: Matcher[(T0, T1, T2), R])(f: (T0, T1, T2) => P): Matcher[P, R] = p =>
    m(p.productElement(0).asInstanceOf[T0], p.productElement(1).asInstanceOf[T1], p.productElement(2).asInstanceOf[T2])

  object MatchersExtras {
    def invert3[R0, R1, R2](t: (R0, R1, R2)): (R2, R1, R0) = (t._3, t._2, t._1)

    def unroll12[R0, R1, R2](t: (R0, (R1, R2))): (R0, R1, R2) = (t._1, t._2._1, t._2._2)

    def unroll21[R0, R1, R2](t: ((R0, R1), R2)): (R0, R1, R2) = (t._1._1, t._1._2, t._2)

    def roll3[R0, R1, R2](t: (R0, R1, R2)): ((R0, R1), R2) = t._1 -> t._2 -> t._3
  }

}
