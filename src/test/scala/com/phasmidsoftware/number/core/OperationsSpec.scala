package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class OperationsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MonadicOperationModulate"
  it should "work" in {
    val z = MonadicOperationModulate
    val (fInt, fBigInt, fRational, fDouble) = z.getFunctions
    fInt(3) shouldBe Success(1)
    fBigInt(3) should matchPattern { case Failure(_) => }
    fRational(Rational(3)) shouldBe Success(Rational.one)
    fDouble(3) shouldBe Success(1.0)
  }

}
