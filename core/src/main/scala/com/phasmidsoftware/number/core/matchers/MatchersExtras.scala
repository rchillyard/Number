/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.matchers

import com.phasmidsoftware.matchers.Matchers

/**
  * This trait extends a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  *
  * CONSIDER why to we need this trait?
  */
trait MatchersExtras extends Matchers {

  //  object MatchersExtras {
  //    def invert3[R0, R1, R2](t: (R0, R1, R2)): (R2, R1, R0) = (t._3, t._2, t._1)
  //
  //    def unroll12[R0, R1, R2](t: (R0, (R1, R2))): (R0, R1, R2) = (t._1, t._2._1, t._2._2)
  //
  //    def unroll21[R0, R1, R2](t: ((R0, R1), R2)): (R0, R1, R2) = (t._1._1, t._1._2, t._2)
  //
  //    def roll3[R0, R1, R2](t: (R0, R1, R2)): ((R0, R1), R2) = t._1 -> t._2 -> t._3
  //  }

}
