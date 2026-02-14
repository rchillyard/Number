/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.AnyContext
import com.phasmidsoftware.number.core.inner.{NatLog, PureNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class ExponentialSpec extends AnyFlatSpec with should.Matchers {

  behavior of "NaturalExponential"

  it should "construct with WholeNumber exponent" in {
    val exp = NaturalExponential(WholeNumber.zero)
    exp.exponent shouldBe WholeNumber.zero
    exp.base shouldBe Real(math.E)
  }

  it should "construct with RationalNumber exponent" in {
    val exp = NaturalExponential(RationalNumber(1, 2))
    exp.exponent shouldBe RationalNumber(1, 2)
  }

  it should "construct with negative exponent" in {
    val exp = NaturalExponential(WholeNumber(-1))
    exp.exponent shouldBe WholeNumber(-1)
  }

  it should "have correct base" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.base shouldBe Real(math.E)
  }

  it should "calculate nominalValue correctly" in {
    NaturalExponential(WholeNumber.zero).nominalValue shouldBe 1.0 +- 1e-10
    NaturalExponential(WholeNumber.one).nominalValue shouldBe math.E +- 1e-10
    NaturalExponential(WholeNumber(2)).nominalValue shouldBe (math.E * math.E) +- 1e-10
    NaturalExponential(WholeNumber(-1)).nominalValue shouldBe (1.0 / math.E) +- 1e-10
  }

  it should "calculate toDouble correctly" in {
    val expZero: Exponential = NaturalExponential(WholeNumber.zero)
    val real = expZero.convert(Real.zero)
    println(s"real = $real")
    expZero.toDouble shouldBe 1.0 +- 1e-10
    NaturalExponential(WholeNumber.one).toDouble shouldBe math.E +- 1e-10
    NaturalExponential(RationalNumber(1, 2)).toDouble shouldBe math.sqrt(math.E) +- 1e-10
  }

  it should "determine isUnity correctly" in {
    NaturalExponential(WholeNumber.zero).isUnity shouldBe true
    NaturalExponential(WholeNumber.one).isUnity shouldBe false
    NaturalExponential(WholeNumber(2)).isUnity shouldBe false
  }

  it should "determine isZero correctly" in {
    NaturalExponential(WholeNumber.zero).isZero shouldBe false
    NaturalExponential(WholeNumber.one).isZero shouldBe false
    NaturalExponential(WholeNumber(-100)).isZero shouldBe false
  }

  it should "determine isExact correctly" in {
    NaturalExponential(WholeNumber.zero).isExact shouldBe true
    NaturalExponential(WholeNumber.one).isExact shouldBe true
    NaturalExponential(RationalNumber(1, 2)).isExact shouldBe true
    NaturalExponential(Real(0.5)).isExact shouldBe false
  }

  it should "determine signum correctly" in {
    NaturalExponential(WholeNumber.zero).signum shouldBe 0
    NaturalExponential(WholeNumber.one).signum shouldBe 1
    NaturalExponential(WholeNumber(-1)).signum shouldBe -1
    NaturalExponential(WholeNumber(5)).signum shouldBe 1
  }

  it should "normalize e^0 to 1" in {
    NaturalExponential(WholeNumber.zero).normalize shouldBe WholeNumber.one
  }

  it should "normalize e^1 stays as NaturalExponential" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.normalize shouldBe exp
  }

  it should "normalize when exponent normalizes" in {
    val exp = NaturalExponential(RationalNumber(2, 2))
    exp.normalize shouldBe NaturalExponential(WholeNumber.one)
  }

  it should "convert to Real" in {
    val exp = NaturalExponential(WholeNumber.one)
    val real = exp.convert(Real.zero)
    real.isDefined shouldBe true
    real.get.toDouble shouldBe math.E +- 1e-10
  }

  it should "convert to same type" in {
    val exp = NaturalExponential(WholeNumber.one)
    val converted = exp.convert(NaturalExponential(WholeNumber.zero))
    converted shouldBe Some(exp)
  }

  it should "return None for unsupported conversion" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.convert(WholeNumber.one) shouldBe None
  }

  it should "compare with other NaturalExponential" in {
    val exp1 = NaturalExponential(WholeNumber.one)
    val exp2 = NaturalExponential(WholeNumber(2))
    val exp3 = NaturalExponential(WholeNumber.one)

    exp1.compare(exp2) should be < 0
    exp2.compare(exp1) should be > 0
    exp1.compare(exp3) shouldBe 0
  }

  it should "compareExact with NaturalExponential" in {
    val exp1: NaturalExponential = NaturalExponential(WholeNumber.one)
    val exp2: NaturalExponential = NaturalExponential(WholeNumber(2))

    exp1.compareExact(exp2) shouldBe Some(-1)
    exp2.compareExact(exp1) shouldBe Some(1)
    exp1.compareExact(exp1) shouldBe Some(0)
  }

  it should "add two NaturalExponentials" in {
    val exp1: NaturalExponential = NaturalExponential(WholeNumber.one)
    val exp2: NaturalExponential = NaturalExponential(WholeNumber(2))
    val result: Exponential = exp1 + exp2

    result.number shouldBe WholeNumber(3)
  }

  it should "render correctly" in {
    NaturalExponential(WholeNumber.zero)(Some("1")).render shouldBe "1"
    NaturalExponential(WholeNumber.one)(Some("e")).render shouldBe "e"
    NaturalExponential(WholeNumber.one).render shouldBe "e"
    NaturalExponential(WholeNumber(2)).render shouldBe "e^2"
    NaturalExponential(WholeNumber(-1)).render shouldBe "e^-1"
    NaturalExponential(RationalNumber(1, 2)).render shouldBe "e^½"
  }

  it should "have correct zero" in {
    val exp: Exponential = NaturalExponential(WholeNumber.one)
    exp.zero.number shouldBe WholeNumber.zero
  }

  it should "throw exception for negate" in {
    val exp = NaturalExponential(WholeNumber.one)
    an[Exception] should be thrownBy exp.negate
  }

  it should "throw exception for unary_-" in {
    val exp = NaturalExponential(WholeNumber.one)
    an[UnsupportedOperationException] should be thrownBy (-exp)
  }

  it should "create unit correctly" in {
    val exp: NaturalExponential = NaturalExponential(WholeNumber.one)
    val unit = exp.unit(WholeNumber(2))

    unit shouldBe a[NaturalExponential]
    unit.number shouldBe WholeNumber(2)
  }

  it should "calculate maybeFactor with NatLog factor context" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.maybeFactor(AnyContext) shouldBe Some(NatLog)
  }

  it should "have scaleFunction that computes e^x" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.scaleFunction(0.0) shouldBe 1.0 +- 1e-10
    exp.scaleFunction(1.0) shouldBe math.E +- 1e-10
    exp.scaleFunction(2.0) shouldBe (math.E * math.E) +- 1e-10
  }

  it should "have derivativeFunction that computes ln(e) * e^x" in {
    val exp = NaturalExponential(WholeNumber.one)
    exp.derivativeFunction(0.0) shouldBe 1.0 +- 1e-10
    exp.derivativeFunction(1.0) shouldBe math.E +- 1e-10
  }

  it should "handle eqv comparison" in {
    val exp1 = NaturalExponential(WholeNumber.one)
    val exp2 = NaturalExponential(WholeNumber.one)
    val exp3 = NaturalExponential(WholeNumber(2))

    exp1.eqv(exp2) shouldBe Success(true)
    exp1.eqv(exp3) shouldBe Success(false)
  }

  it should "handle fuzzyEqv comparison with NaturalExponential" in {
    val exp1 = NaturalExponential(WholeNumber.one)
    val exp2 = NaturalExponential(WholeNumber.one)

    exp1.fuzzyEqv(0.95)(exp2).get shouldBe true
  }

  it should "approximation returns Real" in {
    val exp = NaturalExponential(WholeNumber.one)
    val approx = exp.approximation()

    approx.isDefined shouldBe true
    approx.get.toDouble shouldBe math.E +- 1e-10
  }

  it should "asJavaNumber for exact exponential" in {
    val exp = NaturalExponential(WholeNumber.one)
    val javaNum = exp.asJavaNumber

    javaNum.isDefined shouldBe true
    javaNum.get.doubleValue() shouldBe math.E +- 1e-10
  }

  it should "maybeDouble for exact exponential" in {
    NaturalExponential(WholeNumber.zero).maybeDouble shouldBe Some(1.0)
    NaturalExponential(WholeNumber.one).maybeDouble shouldBe Some(math.E)
  }

  behavior of "BinaryExponential"

  it should "construct with WholeNumber exponent" in {
    val exp: Exponential = BinaryExponential(WholeNumber.zero)
    exp.number shouldBe WholeNumber.zero
    exp.base shouldBe WholeNumber.two
  }

  it should "have correct base" in {
    val exp = BinaryExponential(WholeNumber.one)
    exp.base shouldBe WholeNumber.two
  }

  it should "calculate nominalValue correctly" in {
    BinaryExponential(WholeNumber.zero).nominalValue shouldBe 1.0 +- 1e-10
    BinaryExponential(WholeNumber.one).nominalValue shouldBe 2.0 +- 1e-10
    BinaryExponential(WholeNumber(2)).nominalValue shouldBe 4.0 +- 1e-10
    BinaryExponential(WholeNumber(3)).nominalValue shouldBe 8.0 +- 1e-10
    BinaryExponential(WholeNumber(-1)).nominalValue shouldBe 0.5 +- 1e-10
  }

  it should "calculate toDouble correctly" in {
    BinaryExponential(WholeNumber.zero).toDouble shouldBe 1.0 +- 1e-10
    BinaryExponential(WholeNumber.one).toDouble shouldBe 2.0 +- 1e-10
    BinaryExponential(WholeNumber(10)).toDouble shouldBe 1024.0 +- 1e-10
    BinaryExponential(RationalNumber(1, 2)).toDouble shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "determine isUnity correctly" in {
    BinaryExponential(WholeNumber.zero).isUnity shouldBe true
    BinaryExponential(WholeNumber.one).isUnity shouldBe false
  }

  it should "determine isZero correctly" in {
    BinaryExponential(WholeNumber.zero).isZero shouldBe false
    BinaryExponential(WholeNumber(10)).isZero shouldBe false
  }

  it should "determine isExact correctly" in {
    BinaryExponential(WholeNumber.zero).isExact shouldBe true
    BinaryExponential(WholeNumber.one).isExact shouldBe true
    BinaryExponential(Real(0.5)).isExact shouldBe false
  }

  it should "determine signum correctly" in {
    BinaryExponential(WholeNumber.zero).signum shouldBe 0
    BinaryExponential(WholeNumber.one).signum shouldBe 1
    BinaryExponential(WholeNumber(-1)).signum shouldBe -1
  }

  it should "normalize 2^0 to 1" in {
    BinaryExponential(WholeNumber.zero).normalize shouldBe WholeNumber.one
  }

  it should "normalize 2^1 stays as BinaryExponential" in {
    val exp = BinaryExponential(WholeNumber.one)
    exp.normalize shouldBe exp
  }

  it should "convert to Real" in {
    val exp = BinaryExponential(WholeNumber(3))
    val real = exp.convert(Real.zero)
    real.isDefined shouldBe true
    real.get.toDouble shouldBe 8.0 +- 1e-10
  }

  it should "compareExact with BinaryExponential" in {
    val exp1: Exponential = BinaryExponential(WholeNumber.one)
    val exp2: Exponential = BinaryExponential(WholeNumber(2))

    exp1.compareExact(exp2) shouldBe Some(-1)
    exp2.compareExact(exp1) shouldBe Some(1)
    exp1.compareExact(exp1) shouldBe Some(0)
  }

  it should "render correctly" in {
    BinaryExponential(WholeNumber.zero)(Some("1")).render shouldBe "1"
    BinaryExponential(WholeNumber.one).render shouldBe "2"
    BinaryExponential(WholeNumber(2)).render shouldBe "2^2"
    BinaryExponential(WholeNumber(10)).render shouldBe "2^10"
    BinaryExponential(WholeNumber(-1)).render shouldBe "2^-1"
  }

  it should "create unit correctly" in {
    val exp: Exponential = BinaryExponential(WholeNumber.one)
    val unit = exp.unit(WholeNumber(5))

    unit shouldBe a[BinaryExponential]
    unit.number shouldBe WholeNumber(5)
  }

  it should "throw exception for negate" in {
    val exp = BinaryExponential(WholeNumber.one)
    an[Exception] should be thrownBy exp.negate
  }

  it should "have scaleFunction that computes 2^x" in {
    val exp = BinaryExponential(WholeNumber.one)
    exp.scaleFunction(0.0) shouldBe 1.0 +- 1e-10
    exp.scaleFunction(1.0) shouldBe 2.0 +- 1e-10
    exp.scaleFunction(3.0) shouldBe 8.0 +- 1e-10
    exp.scaleFunction(10.0) shouldBe 1024.0 +- 1e-10
  }

  it should "have derivativeFunction that computes ln(2) * 2^x" in {
    val exp = BinaryExponential(WholeNumber.one)
    val ln2 = math.log(2.0)
    exp.derivativeFunction(0.0) shouldBe ln2 +- 1e-10
    exp.derivativeFunction(1.0) shouldBe (ln2 * 2.0) +- 1e-10
  }

  it should "maybeFactor returns PureNumber" in {
    val exp = BinaryExponential(WholeNumber.one)
    exp.maybeFactor(AnyContext) shouldBe Some(PureNumber)
  }

  behavior of "Exponential companion object"

  it should "provide NaturalExponential.one constant" in {
    NaturalExponential.one.number shouldBe WholeNumber.zero
    NaturalExponential.one.isUnity shouldBe true
  }

  it should "provide NaturalExponential.e constant" in {
    NaturalExponential.e.number shouldBe WholeNumber.one
    NaturalExponential.e.render shouldBe "e"
  }

  it should "combine NaturalExponentials via CommutativeMonoid" in {
    import Exponential.ExponentialIsCommutativeMonoid

    val exp1 = NaturalExponential(WholeNumber.one)
    val exp2 = NaturalExponential(WholeNumber(2))
    val result = ExponentialIsCommutativeMonoid.combine(exp1, exp2)

    result.number shouldBe WholeNumber(3)
  }

  it should "have empty as NaturalExponential(0)" in {
    import Exponential.ExponentialIsCommutativeMonoid

    ExponentialIsCommutativeMonoid.empty.number shouldBe WholeNumber.zero
  }

  it should "throw exception when combining different exponential types" in {
    import Exponential.ExponentialIsCommutativeMonoid

    val nat = NaturalExponential(WholeNumber.one)
    val bin = BinaryExponential(WholeNumber.one)

    an[Exception] should be thrownBy {
      ExponentialIsCommutativeMonoid.combine(nat, bin)
    }
  }

  behavior of "Exponential edge cases"

  it should "handle very large exponents" in {
    val exp = NaturalExponential(WholeNumber(100))
    exp.isExact shouldBe true
    exp.toDouble should be > 1e40
  }

  it should "handle very small (negative) exponents" in {
    val exp = NaturalExponential(WholeNumber(-100))
    exp.isExact shouldBe true
    exp.toDouble should be < 1e-40
  }

  it should "handle fractional exponents" in {
    val exp = NaturalExponential(RationalNumber(1, 3))
    exp.isExact shouldBe true
    exp.toDouble shouldBe math.pow(math.E, 1.0 / 3.0) +- 1e-10
  }

  it should "handle renderNumber for various exponents" in {
    NaturalExponential(WholeNumber.one).renderNumber shouldBe ""
    NaturalExponential(WholeNumber(2)).renderNumber shouldBe "^2"
    NaturalExponential(RationalNumber(1, 2)).renderNumber shouldBe "^½"
  }
}