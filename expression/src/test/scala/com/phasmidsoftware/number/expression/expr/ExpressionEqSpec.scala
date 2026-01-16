package com.phasmidsoftware.number.expression.expr

/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

import cats.Eq
import com.phasmidsoftware.number.algebra.eager.WholeNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpressionEqSpec extends AnyFlatSpec with Matchers {

  behavior of "Eq[Expression]"

  it should "recognize equal atomic expressions" in {
    val e1 = Literal(WholeNumber(5))
    val e2 = Literal(WholeNumber(5))
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize unequal atomic expressions" in {
    val e1 = Literal(WholeNumber(5))
    val e2 = Literal(WholeNumber(7))
    Eq[Expression].eqv(e1, e2) shouldBe false
  }

  it should "recognize equal UniFunction expressions" in {
    val e1 = UniFunction(Literal(WholeNumber(2)), Ln)
    val e2 = UniFunction(Literal(WholeNumber(2)), Ln)
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize unequal UniFunction expressions with different operands" in {
    val e1 = UniFunction(Literal(WholeNumber(2)), Ln)
    val e2 = UniFunction(Literal(WholeNumber(3)), Ln)
    Eq[Expression].eqv(e1, e2) shouldBe false
  }

  it should "recognize unequal UniFunction expressions with different functions" in {
    val e1 = UniFunction(Literal(WholeNumber(2)), Ln)
    val e2 = UniFunction(Literal(WholeNumber(2)), Exp)
    Eq[Expression].eqv(e1, e2) shouldBe false
  }

  it should "recognize equal BiFunction expressions with commutative operations" in {
    val a = Literal(WholeNumber(3))
    val b = Literal(WholeNumber(5))
    val e1 = BiFunction(a, b, Sum)
    val e2 = BiFunction(b, a, Sum)
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize equal BiFunction product expressions regardless of order" in {
    val a = Literal(WholeNumber(3))
    val b = Literal(WholeNumber(5))
    val e1 = BiFunction(a, b, Product)
    val e2 = BiFunction(b, a, Product)
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize unequal BiFunction expressions with non-commutative operations" in {
    val a = Literal(WholeNumber(3))
    val b = -Literal(WholeNumber(5))
    val e1 = BiFunction(a, b, Sum)
    val e2 = BiFunction(b, a, Sum)
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize equal BiFunction division expressions only in same order" in {
    val a = Literal(WholeNumber(10))
    val b = Literal(WholeNumber(2)).reciprocal
    val e1 = BiFunction(a, b, Product)
    val e2 = BiFunction(a, b, Product)
    val e3 = BiFunction(b, a, Product)
    Eq[Expression].eqv(e1, e2) shouldBe true
    Eq[Expression].eqv(e1, e3) shouldBe true
  }

  it should "recognize equal BiFunction power expressions only in same order" in {
    val a = Literal(WholeNumber(2))
    val b = Literal(WholeNumber(3))
    val e1 = BiFunction(a, b, Power)
    val e2 = BiFunction(a, b, Power)
    val e3 = BiFunction(b, a, Power)
    Eq[Expression].eqv(e1, e2) shouldBe true
    Eq[Expression].eqv(e1, e3) shouldBe false
  }

  ignore should "recognize equal Aggregate expressions with commutative operations" in {
    val a = Literal(WholeNumber(1))
    val b = Literal(WholeNumber(2))
    val c = Literal(WholeNumber(3))
    val e1 = Aggregate(Sum, Seq(a, b, c))
    val e2 = Aggregate(Sum, Seq(c, a, b))
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  ignore should "recognize equal Aggregate product expressions regardless of order" in {
    val a = Literal(WholeNumber(2))
    val b = Literal(WholeNumber(3))
    val c = Literal(WholeNumber(5))
    val e1 = Aggregate(Product, Seq(a, b, c))
    val e2 = Aggregate(Product, Seq(b, c, a))
    Eq[Expression].eqv(e1, e2) shouldBe true
  }

  it should "recognize unequal Aggregate expressions with different lengths" in {
    val a = Literal(WholeNumber(1))
    val b = Literal(WholeNumber(2))
    val c = Literal(WholeNumber(3))
    val e1 = Aggregate(Sum, Seq(a, b))
    val e2 = Aggregate(Sum, Seq(a, b, c))
    Eq[Expression].eqv(e1, e2) shouldBe false
  }

  it should "recognize unequal expressions of different types" in {
    val a = Literal(WholeNumber(5))
    val b = UniFunction(Literal(WholeNumber(5)), Ln)
    Eq[Expression].eqv(a, b) shouldBe false
  }

  it should "work with cats syntax ===" in {
    val e1 = Literal(WholeNumber(42))
    val e2 = Literal(WholeNumber(42))
    val e3 = Literal(WholeNumber(7))

    (e1 === e2) shouldBe true
    (e1 === e3) shouldBe false
  }

  it should "handle nested expressions with commutative operations" in {
    val a = Literal(WholeNumber(2))
    val b = Literal(WholeNumber(3))
    val sum1 = BiFunction(a, b, Sum)
    val sum2 = BiFunction(b, a, Sum)
    val prod1 = BiFunction(sum1, Literal(WholeNumber(5)), Product)
    val prod2 = BiFunction(Literal(WholeNumber(5)), sum2, Product)

    Eq[Expression].eqv(prod1, prod2) shouldBe true
  }

  it should "handle complex nested expressions" in {
    // (2 + 3) * 5 === 5 * (3 + 2)
    val a = Literal(WholeNumber(2))
    val b = Literal(WholeNumber(3))
    val c = Literal(WholeNumber(5))

    val e1 = BiFunction(BiFunction(a, b, Sum), c, Product)
    val e2 = BiFunction(c, BiFunction(b, a, Sum), Product)

    Eq[Expression].eqv(e1, e2) shouldBe true
  }
}
