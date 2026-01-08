package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.dimensions.core.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TRationalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "TRational Addition"

  it should "add 1/2 + 1/2 = 1/1" in {
    // Now normalized!
    summon[AddTRat[TRat[1, 2], TRat[1, 2]] =:= TRat[1, 1]]
  }

  it should "add 1/2 + 0/1 = 1/2" in {
    summon[AddTRat[TRat[1, 2], TRat[0, 1]] =:= TRat[1, 2]]
  }

  behavior of "TRational Multiplication"

  it should "multiply 1/2 * 2/1 = 1/1" in {
    summon[MulTRat[TRat[1, 2], TRat[2, 1]] =:= TRat[1, 1]]
  }

  it should "multiply 1/2 * 1/2 = 1/4" in {
    summon[MulTRat[TRat[1, 2], TRat[1, 2]] =:= TRat[1, 4]]
  }

  behavior of "TRational Subtraction"

  it should "subtract 1/1 - 1/2 = 1/2" in {
    summon[SubTRat[TRat[1, 1], TRat[1, 2]] =:= TRat[1, 2]]
  }

  behavior of "TRational Division"

  it should "divide 1/2 / 1/2 = 1/1" in {
    summon[DivTRat[TRat[1, 2], TRat[1, 2]] =:= TRat[1, 1]]
  }
}