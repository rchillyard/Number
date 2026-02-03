package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Eager, Real}
import com.phasmidsoftware.number.core.inner.{Inverse, Rational}
import com.phasmidsoftware.number.core.inner.Rational.RationalOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InversePowerSpec extends AnyFlatSpec with Matchers {

  behavior of "Number raised to inverse integer powers"

  it should "evaluate cube root via 1/3 power" in {
    val expr = Expression(8) ∧ 3.invert
    expr.materialize shouldBe Eager(2)
  }

  it should "evaluate square root via 1/2 power" in {
    val expr = Expression((4)) ∧ Rational.half
    expr.materialize shouldBe Eager(2)
  }

  it should "evaluate fourth root via 1/4 power" in {
    val expr = Expression((16)) ∧ 4.invert
    val materialized = expr.materialize
    materialized shouldBe Eager(2)
  }

  it should "handle fifth root" in {
    val expr = Expression((32)) ∧ 5.invert
    expr.materialize.normalize shouldBe Eager(2)
  }

  it should "evaluate 27^(1/3)" in {
    val expr = Expression((27)) ∧ 3.invert
    expr.materialize shouldBe Eager(3)
  }

  it should "stay symbolic for non-exact roots" in {
    val expr = Expression(7) ∧ 3.invert
    // Should remain symbolic until forced to PureNumber
    expr shouldBe a[Expression]
    // When materialized, should be approximately 1.913
    val result = expr.materialize.fuzzy
    result match {
      case r: Real =>
        r.value should be(1.913 +- 0.001)
      case _ =>
        fail("Should produce Real for non-exact root")
    }
  }

  it should "handle negative radicands with odd roots" in {
    //    val expr = Expression(-8) ∧ Rational(1, 3)
    //    expr.materialize shouldBe Eager(-2)
    pending
  }

  it should "handle rational bases" in {
    //    val expr = Expression(Rational(8, 27)) ∧ Rational(3).invert
    //    expr.materialize shouldBe Rational(2, 3)
    pending
  }

  it should "handle unit fractions for higher roots" in {
    val expr = Expression(64) ∧ 6.invert
    expr.materialize shouldBe Eager(2)
  }

  behavior of "InverseInt extractor"

  it should "match unit fractions" in {
    Rational.half match {
      case Inverse(n) => n shouldBe 2
      case _ => fail("Should match 1/2")
    }
  }

  it should "match 1/3" in {
    Rational(1, 3) match {
      case Inverse(n) => n shouldBe 3
      case _ => fail("Should match 1/3")
    }
  }

  it should "not match non-unit numerators" in {
    Rational(2, 3) match {
      case Inverse(_) => fail("Should not match 2/3")
      case _ => succeed
    }
  }

  it should "not match integers" in {
    Rational(2, 1) match {
      case Inverse(_) => fail("Should not match integer 2")
      case _ => succeed
    }
  }

  it should "match 1/10" in {
    Rational(1, 10) match {
      case Inverse(n) => n shouldBe 10
      case _ => fail("Should match 1/10")
    }
  }

  behavior of "Expression power operations with inverse integers"

  it should "simplify (√3)^2 to 3 using inverse and forward powers" in {
    val sqrt3 = Expression(3) ∧ Rational.half
    val squared = sqrt3 ∧ Two
    squared.materialize shouldBe Eager(3)
  }

  it should "handle chained root operations" in {
    // (16^(1/4))^(1/2) = 16^(1/8) but also = (16^(1/2))^(1/4) = 4^(1/4)
    val expr = (Expression(16) ∧ 4.invert) ∧ Rational.half
    // This is the 8th root of 16, which is √2
    val result = expr.materialize
    result match {
      case r: Real => r.value should be(1.414 +- 0.001)
      case _ => // Could also be exact symbolic form
    }
  }

  it should "work with larger perfect powers" in {
    val expr = Expression(1024) ∧ 10.invert
    expr.materialize shouldBe Eager(2)
  }

  it should "handle zero correctly (by throwing an exception)" in {
    val expr = Expression(0) ∧ 3.invert
    a[ExpressionException] should be thrownBy (expr.materialize)
  }

  it should "handle one correctly" in {
    val expr = Expression(1) ∧ 100.invert
    expr.materialize shouldBe Eager(1)
  }
}