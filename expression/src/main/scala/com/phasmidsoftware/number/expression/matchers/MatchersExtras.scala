/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.expression.matchers

import com.phasmidsoftware.matchers.Matchers

/**
  * This trait extends a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  *
  * CONSIDER why to we need this trait?
  */
trait MatchersExtras extends Matchers {


  /**
    * Creates a `MatchResult` by applying a given function `f` to an input value of type `T`, 
    * and matching the result if the function yields a defined `Option`.
    * Matches and processes a given input using a function that returns an Option.
    * TODO promote this into `Matchers` (where `matchOptionFunc` is already defined but could be based on this method).
    *
    * @param f a function that transforms an input of type `T` into an `Option[R]`.
    * @param t the input value of type `T` to be transformed and matched.
    * @tparam T the input type.
    * @tparam R the result type contained in the `Option`.
    * @return a `MatchResult[R]` constructed from the result of the function if the `Option` is defined.
    */
  def matchOptionFunc2[T, R](f: T => Option[R])(t: T): MatchResult[R] =
    matchIfDefined(f(t))(t)

  object MatchersExtras {
    def invert3[R0, R1, R2](t: (R0, R1, R2)): (R2, R1, R0) = (t._3, t._2, t._1)

    def unroll12[R0, R1, R2](t: (R0, (R1, R2))): (R0, R1, R2) = (t._1, t._2._1, t._2._2)

    def unroll21[R0, R1, R2](t: ((R0, R1), R2)): (R0, R1, R2) = (t._1._1, t._1._2, t._2)

    def roll3[R0, R1, R2](t: (R0, R1, R2)): ((R0, R1), R2) = t._1 -> t._2 -> t._3
  }

}
