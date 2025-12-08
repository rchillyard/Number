package com.phasmidsoftware.number.algebra

import algebra.ring.Ring
import com.phasmidsoftware.number.algebra.Real.realIsRing
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RealSpec extends AnyFlatSpec with Matchers {

  private val rr: Ring[Real] = implicitly[Ring[Real]]

  behavior of "Real"

  it should "apply(String)" in {
    val x = Real("1.5")
    val y = Real("2")
    val z = Real("3.1415927(31)")
  }

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x = Real(1)
    val y = Real(2)
    rr.plus(x, y) compareTo RationalNumber(3) shouldBe 0
  }

  it should "perform subtraction correctly" in {
    val x = Real(5)
    val y = Real(3)
    rr.plus(x, realIsRing.negate(y)) should matchPattern { case Real(2, _) => }
  }

  it should "perform multiplication correctly" in {
    val x = Real(2)
    val y = Real(3)
    rr.times(x, y) should matchPattern { case Real(6, _) => }
  }

  it should "perform division correctly" in {
    val x = Real(6)
    val y = Real(2)
    realIsRing.div(x, y) should matchPattern { case Real(3, _) => }
  }

  // Comparison operations
  it should "compare numbers correctly A" in {
    val x = Real(1)
    val y = Real(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }
  it should "compare numbers correctly B" in {
    val x = Real(1.5)
    val y = Real(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = Real(5)
    x.convert(Real.zero) shouldBe Some(x)
    x.convert(Angle.zero) shouldBe None
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    val x = Real(0)
    x.isZero shouldBe true
  }

  behavior of "Ring[Real]"

  it should "plus" in {
    rr.plus(Real(3), Angle.pi.approximation(true).get) shouldBe Real(6.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box)))
  }
  it should "negate" in {
    rr.negate(Real(3)) shouldBe Real(-3)
  }

}
