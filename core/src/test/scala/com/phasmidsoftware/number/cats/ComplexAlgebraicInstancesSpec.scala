package com.phasmidsoftware.number.cats

import algebra.ring.CommutativeRing
import com.phasmidsoftware.number.core.numerical.{Complex, ComplexCartesian, Number}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComplexAlgebraicInstancesSpec extends AnyFlatSpec with Matchers with ComplexAlgebraicInstances {
  behavior of "complexCommutativeRing (zero/one/fromInt/plus/times/negate)"

  ignore should "provide zero/one/fromInt/plus/times/negate" in {
    val CR: CommutativeRing[Complex] = complexCommutativeRing

    // zero/one
    CR.zero shouldBe ComplexCartesian(Number.zero, Number.zero)
    CR.one shouldBe Complex.unit

    // fromInt
    CR.fromInt(2) shouldBe Complex(Number(2))

    // plus/times/negate
    CR.plus(ComplexCartesian(1, 2), ComplexCartesian(3, 4)) shouldBe ComplexCartesian(4, 6)
    CR.times(ComplexCartesian(1, 2), ComplexCartesian(3, 4)) shouldBe ComplexCartesian(-5, 10)
    CR.negate(ComplexCartesian(3, -2)) shouldBe ComplexCartesian(-3, 2)
  }
}


