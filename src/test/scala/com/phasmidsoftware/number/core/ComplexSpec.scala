package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Complex"

  private val c1_2 = ComplexCartesian(Number.one, Number.two)
  private val c2_0 = ComplexCartesian(Number.two, Number.zero)

  it should "imag" in {

  }

  it should "compare" in {

  }

  it should "multiply" in {
    val z1 = c1_2 * c2_0
    z1.materialize shouldBe ComplexCartesian(Number.two, Number(4))
  }

  it should "unary_$minus" in {

  }

  it should "asNumber" in {

  }

  it should "divide" in {
    val z = ComplexCartesian(Number.two, Number(4))
    val z1 = z / c2_0
    z1.materialize shouldBe c1_2

  }

  it should "isAtomic" in {

  }

  it should "simplify" in {

  }

  it should "real" in {

  }

  it should "isExact" in {

  }

  it should "power" in {

  }

  it should "add" in {
    val c3 = c1_2 add c2_0
    c3 should matchPattern { case ComplexCartesian(ExactNumber(Right(3), Scalar), ExactNumber(Right(2), Scalar)) => }
  }

  it should "depth" in {

  }

  it should "materialize" in {

  }

  it should "maybeFactor" in {

  }

  it should "sum" in {

  }

  it should "showImaginary" in {

  }

  it should "doAdd" in {

  }

  it should "invert" in {

  }

  it should "apply" in {

  }

  it should "convertToPolar" in {

  }

  it should "unapply" in {

  }

  it should "asComplex" in {

  }

  it should "convertToCartesian" in {

  }

}
