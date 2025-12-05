/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.{Angle, Eager, RationalNumber, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EmptySpec extends AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps

  behavior of "∅"
  it should "yield 42a" in {
    ∅ + 42 shouldBe Literal(Eager(42))
  }
  it should "yield 42b" in {
    ((∅ + 6) :+ 36) shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(36), Some("36")), Sum)
  }
  it should "yield 42c" in {
    (∅ + 6 :+ 36) shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(36), Some("36")), Sum)
  }
  it should "yield 4a" in {
    val expression = ∅ + 6 * Rational(2, 3)
    expression.materialize shouldBe Eager(4)
  }
  it should "yield 4b" in {
    val expression = (∅ + 6) :* RationalNumber(2, 3)
    expression.materialize shouldBe Eager(4)
  }
  it should "yield 4c" in {
    import Expression.ExpressionOps
    val expression = ∅ + 6 :* Literal(RationalNumber(2, 3))
    expression.materialize shouldBe Eager(4)
  }
  it should "yield 5a" in {
    import Expression.ExpressionOps
    val expression = ∅ + 6 :* Literal(RationalNumber(2, 3)) :+ One
    expression.materialize shouldBe Eager(5)
  }
  it should "NOT yield 10" in {
    import Expression.ExpressionOps
    val expression = ∅ + 6 :* Literal(RationalNumber(2, 3)) :+ One
    expression.materialize shouldBe Eager(5)
  }
  it should "use implicit materialization" in {
    import Expression.*
    val expression = ∅ + 6 :* Literal(RationalNumber(2, 3)) :+ One
    val eager: Eager = expression
    eager shouldBe Eager(5)
  }
}
