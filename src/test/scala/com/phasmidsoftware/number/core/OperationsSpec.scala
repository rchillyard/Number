package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class OperationsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MonadicOperationModulate"
  it should "work" in {
    val z = MonadicOperationModulate(-1, 1, circular = false)
    val (fInt, fRational, fDouble) = z.functions
    fInt(3) shouldBe Success(1)
    fRational(Rational(3)) shouldBe Success(Rational.one)
    fDouble(3) shouldBe Success(1.0)
  }
  it should "work with circular true" in {
    val z = MonadicOperationModulate(-1, 1, circular = true)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(1)
  }
  it should "work with circular false" in {
    val z = MonadicOperationModulate(-1, 1, circular = false)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }

}
