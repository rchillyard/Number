package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Rational.RationalHelper
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
    c2_0.isAtomic shouldBe true
  }

  it should "simplify" in {

  }

  it should "real" in {

  }

  it should "isExact" in {

  }

  it should "power1" in {
    val z = c1_2 power 2
    z.materialize should matchPattern { case ComplexCartesian(ExactNumber(Right(-3), Scalar), ExactNumber(Right(4), Scalar)) => }
  }

  it should "power2" in {
    val z = c1_2 power 0
    z.materialize should matchPattern { case ComplexCartesian(ExactNumber(Right(1), Scalar), ExactNumber(Right(0), Scalar)) => }
  }

  it should "power3" in {
    val z = c1_2 power -1
    z.materialize shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
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
    val z = c1_2.invert
    z.materialize shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "apply" in {

  }

  it should "convertToPolar" in {

  }

  it should "unapply" in {

  }

  it should "narrow" in {

  }

  it should "convertToCartesian" in {

  }

}
