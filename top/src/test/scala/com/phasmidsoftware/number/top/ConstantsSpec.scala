package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.{Real, WholeNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ConstantsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Constants"

  it should "c" in {
    val c = Constants.c
    c.value shouldBe WholeNumber(299792458)
  }

  ignore should "G" in {
    Constants.Gmaybe match {
      case Left(value) =>
        fail(s"Expected Right(value), got Left($value)")
      case Right(g) =>
        g.value shouldBe Real(1)
    }

  }
}
