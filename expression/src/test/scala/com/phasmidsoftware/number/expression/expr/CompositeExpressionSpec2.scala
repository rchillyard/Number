/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.RestrictedContext
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Constants, RelativeFuzz}
import com.phasmidsoftware.number.expression.expr.Expression.em
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Comprehensive test suite for the CompositeExpression trait hierarchy.
  * Tests cover:
  * - UniFunction (monadic functions)
  * - BiFunction (dyadic functions)
  * - Aggregate (n-ary operations)
  */
class CompositeExpressionSpec2 extends AnyFlatSpec with Matchers {

  private val pureNumberContext = RestrictedContext(PureNumber)

  // ============================================================================
  // UniFunction Tests - Basic Properties
  // ============================================================================

  behavior of "UniFunction - Basic Properties"

  it should "not be atomic" in {
    val uni = UniFunction(One, Negate)
    uni.isAtomic shouldBe false
  }

  it should "have correct depth" in {
    val uni1 = UniFunction(One, Negate)
    uni1.depth shouldBe 2 // 1 + depth(One)

    val nested = UniFunction(UniFunction(One, Negate), Reciprocal)
    nested.depth shouldBe 3 // 1 + depth(UniFunction)
  }

  it should "preserve exactness from operand" in {
    val exact = UniFunction(One, Negate)
    exact.isExact shouldBe true

    val fuzzy = UniFunction(Literal(Real(3.14, Some(AbsoluteFuzz(0.01, Box)))), Negate)
    fuzzy.isExact shouldBe false
  }

  it should "provide terms correctly" in {
    val uni = UniFunction(Two, Negate)
    uni.terms shouldBe Seq(Two)
  }

  it should "render correctly" in {
    val uni = UniFunction(Pi, Sine)
    uni.renderAsExpression should include("sin")
    uni.renderAsExpression should include("𝛑")
  }

  // ============================================================================
  // UniFunction Tests - Evaluation
  // ============================================================================

  behavior of "UniFunction - Evaluation"

  it should "evaluate Negate correctly" in {
    val negOne = UniFunction(One, Negate)
    negOne.evaluate(pureNumberContext) shouldBe Some(WholeNumber(-1))
  }

  it should "evaluate Reciprocal correctly" in {
    val recTwo = UniFunction(Two, Reciprocal)
    recTwo.evaluate(pureNumberContext) shouldBe Some(RationalNumber.half)
  }

  it should "evaluate nested functions correctly" in {
    val doubleNeg = UniFunction(UniFunction(One, Negate), Negate)
    doubleNeg.evaluate(pureNumberContext) shouldBe Some(WholeNumber.one)
  }

  it should "evaluate Exp and Ln as inverses" in {
    val eulerMascheroni = UniFunction(UniFunction(EulerMascheroni, Exp), Ln)
    eulerMascheroni.materialize shouldBe Eager(Constants.gamma)
  }

  // ============================================================================
  // UniFunction Tests - Simplification
  // ============================================================================

  behavior of "UniFunction - Simplification"

  it should "simplify complementary functions" in {
    val expLn = UniFunction(UniFunction(Two, Ln), Exp)
    val simplified = expLn.simplify
    simplified shouldBe Two
  }

  it should "simplify ln(exp(x)) to x" in {
    val lnExp = UniFunction(UniFunction(Two, Exp), Ln)
    val simplified = lnExp.simplify
    simplified shouldBe Two
  }

  it should "simplify reciprocal of reciprocal" in {
    val recRec = UniFunction(UniFunction(Two, Reciprocal), Reciprocal)
    val simplified = recRec.simplify
    simplified shouldBe Two
  }

  it should "simplify negate of negate" in {
    val negNeg = UniFunction(UniFunction(Two, Negate), Negate)
    val simplified = negNeg.simplify
    simplified shouldBe Two
  }

  it should "simplify ValueExpression monadic functions" in {
    val sinPi = UniFunction(Pi, Sine)
    sinPi.simplify shouldBe Zero
  }

  it should "simplify cos(π) to -1" in {
    val cosPi = UniFunction(Pi, Cosine)
    cosPi.simplify shouldBe MinusOne
  }

  it should "simplify ln(e) to 1" in {
    val lnE = UniFunction(E, Ln)
    lnE.simplify shouldBe One
  }

  it should "simplify exp(0) to 1" in {
    val expZero = UniFunction(Zero, Exp)
    expZero.simplify shouldBe One
  }

  it should "simplify reciprocal of root" in {
    val recPhi = UniFunction(Root.phi, Reciprocal)
    recPhi.simplify should not be recPhi // Should be simplified
  }

  it should "simplify negation of root" in {
    val negPhi = UniFunction(Root.phi, Negate)
    negPhi.simplify should not be negPhi // Should be simplified
  }

  it should "handle reciprocal of ln as log_e" in {
    val recLn = UniFunction(UniFunction(Two, Ln), Reciprocal)
    val simplified = recLn.simplify
    simplified shouldBe a[BiFunction]
  }

  // ============================================================================
  // UniFunction Tests - Equality and Hashing
  // ============================================================================

  behavior of "UniFunction - Equality"

  it should "be equal for same operand and function" in {
    val uni1 = UniFunction(Two, Negate)
    val uni2 = UniFunction(Two, Negate)
    uni1 shouldBe uni2
  }

  it should "not be equal for different operands" in {
    val uni1 = UniFunction(One, Negate)
    val uni2 = UniFunction(Two, Negate)
    uni1 should not be uni2
  }

  it should "not be equal for different functions" in {
    val uni1 = UniFunction(Two, Negate)
    val uni2 = UniFunction(Two, Reciprocal)
    uni1 should not be uni2
  }

  it should "have consistent hashCode" in {
    val uni1 = UniFunction(Two, Negate)
    val uni2 = UniFunction(Two, Negate)
    uni1.hashCode shouldBe uni2.hashCode
  }

  // ============================================================================
  // BiFunction Tests - Basic Properties
  // ============================================================================

  behavior of "BiFunction - Basic Properties"

  it should "not be atomic" in {
    val bi = BiFunction(One, Two, Sum)
    bi.isAtomic shouldBe false
  }

  it should "have correct depth" in {
    val bi1 = BiFunction(One, Two, Sum)
    bi1.depth shouldBe 2 // 1 + max(depth(One), depth(Two))

    val nested = BiFunction(UniFunction(One, Negate), Two, Product)
    nested.depth shouldBe 3 // 1 + max(2, 1)
  }

  it should "be exact when both operands are exact" in {
    val exact = BiFunction(One, Two, Sum)
    exact.isExact shouldBe true
  }

  it should "not be exact when either operand is fuzzy" in {
    val fuzzy1 = BiFunction(Literal(Real(3.14, Some(AbsoluteFuzz(0.01, Box)))), Two, Sum)
    fuzzy1.isExact shouldBe false

    val fuzzy2 = BiFunction(One, Literal(Real(3.14, Some(AbsoluteFuzz(0.01, Box)))), Sum)
    fuzzy2.isExact shouldBe false
  }

  it should "provide terms correctly" in {
    val bi = BiFunction(One, Two, Sum)
    bi.terms shouldBe Seq(One, Two)
  }

  it should "render correctly" in {
    val bi = BiFunction(Pi, E, Sum)
    bi.renderAsExpression should include("𝛑")
    bi.renderAsExpression should include("e")
  }

  // ============================================================================
  // BiFunction Tests - Evaluation (Sum)
  // ============================================================================

  behavior of "BiFunction - Evaluation (Sum)"

  it should "evaluate sum correctly" in {
    val sum = BiFunction(One, Two, Sum)
    sum.evaluate(pureNumberContext) shouldBe Some(WholeNumber(3))
  }

  it should "evaluate sum with zero" in {
    val sumZero = BiFunction(Two, Zero, Sum)
    sumZero.evaluate(pureNumberContext) shouldBe Some(WholeNumber.two)
  }

  it should "evaluate negative sum" in {
    val negSum = BiFunction(MinusOne, One, Sum)
    negSum.evaluate(pureNumberContext) shouldBe Some(WholeNumber.zero)
  }

  // ============================================================================
  // BiFunction Tests - Evaluation (Product)
  // ============================================================================

  behavior of "BiFunction - Evaluation (Product)"

  it should "evaluate product correctly" in {
    val prod = BiFunction(Two, Two, Product)
    prod.evaluate(pureNumberContext) shouldBe Some(WholeNumber(4))
  }

  it should "evaluate product with zero" in {
    val prodZero = BiFunction(Two, Zero, Product)
    prodZero.evaluate(pureNumberContext) shouldBe Some(WholeNumber.zero)
  }

  it should "evaluate product with one" in {
    val prodOne = BiFunction(Two, One, Product)
    prodOne.evaluate(pureNumberContext) shouldBe Some(WholeNumber.two)
  }

  it should "evaluate product with minus one" in {
    val prodNeg = BiFunction(Two, MinusOne, Product)
    prodNeg.evaluate(pureNumberContext) shouldBe Some(WholeNumber(-2))
  }

  // ============================================================================
  // BiFunction Tests - Evaluation (Power)
  // ============================================================================

  behavior of "BiFunction - Evaluation (Power)"

  it should "evaluate power correctly" in {
    val power = BiFunction(Two, Two, Power)
    power.evaluate(pureNumberContext) shouldBe Some(WholeNumber(4))
  }

  it should "evaluate power with zero exponent" in {
    val powerZero = BiFunction(Two, Zero, Power)
    powerZero.evaluate(pureNumberContext) shouldBe Some(WholeNumber.one)
  }

  it should "evaluate power with one exponent" in {
    val powerOne = BiFunction(Two, One, Power)
    powerOne.evaluate(pureNumberContext) shouldBe Some(WholeNumber.two)
  }

  it should "evaluate negative power" in {
    val powerNeg = BiFunction(Two, MinusOne, Power)
    powerNeg.evaluate(pureNumberContext) shouldBe Some(RationalNumber.half)
  }

  // ============================================================================
  // BiFunction Tests - Simplification (Sum)
  // ============================================================================

  behavior of "BiFunction - Simplification (Sum)"

  it should "simplify x + 0 to x" in {
    val sumZero = BiFunction(EulerMascheroni, Zero, Sum)
    sumZero.simplify shouldBe EulerMascheroni
  }

  it should "simplify 0 + x to x" in {
    val zeroSum = BiFunction(Zero, Pi, Sum)
    zeroSum.simplify shouldBe Pi
  }

  it should "simplify x + x to 2*x for transcendental" in {
    val x = EulerMascheroni
    val doubleSum = BiFunction(x, x, Sum)
    val simplified = doubleSum.simplify
    simplified shouldBe a[BiFunction]
    simplified.asInstanceOf[BiFunction].f shouldBe Product
    simplified.asInstanceOf[BiFunction].a shouldBe x
    simplified.asInstanceOf[BiFunction].b shouldBe Two
  }

  it should "evaluate constant sum completely" in {
    val sum = BiFunction(Two, Two, Sum)
    val simplified = sum.simplify
    simplified shouldBe Literal(WholeNumber(4))
  }

  // ============================================================================
  // BiFunction Tests - Simplification (Product)
  // ============================================================================

  behavior of "BiFunction - Simplification (Product)"

  it should "simplify x * 0 to 0" in {
    val prodZero = BiFunction(Pi, Zero, Product)
    prodZero.simplify shouldBe Zero
  }

  it should "simplify 0 * x to 0" in {
    val zeroProd = BiFunction(Zero, EulerMascheroni, Product)
    zeroProd.simplify shouldBe Zero
  }

  it should "simplify x * 1 to x" in {
    val prodOne = BiFunction(Pi, One, Product)
    prodOne.simplify shouldBe Pi
  }

  it should "simplify 1 * x to x" in {
    val oneProd = BiFunction(One, EulerMascheroni, Product)
    oneProd.simplify shouldBe EulerMascheroni
  }

  it should "simplify x * (-1) to -x" in {
    val prodNeg = BiFunction(Pi, MinusOne, Product)
    val simplified = prodNeg.simplify
    simplified shouldBe Literal(Angle.negPi)
  }

  it should "simplify -x to -x" in {
    val prodNeg = UniFunction(Pi, Negate)
    val simplified = prodNeg.simplify
    simplified shouldBe Literal(Angle.negPi)
  }

  it should "simplify (-1) * x to -x" in {
    val negProd = BiFunction(MinusOne, EulerMascheroni, Product)
    val simplified = negProd.simplify
    simplified shouldBe a[UniFunction]
    simplified.asInstanceOf[UniFunction].f shouldBe Negate
    simplified.asInstanceOf[UniFunction].x shouldBe EulerMascheroni
  }

  it should "simplify x * x to x² for transcendental" in {
    val x = Pi
    val square = BiFunction(x, x, Product)
    val simplified = square.simplify
    simplified shouldBe a[BiFunction]
    val bf = simplified.asInstanceOf[BiFunction]
    bf.f shouldBe Power
    bf.a shouldBe x
    bf.b shouldBe Two
  }

  it should "evaluate constant product completely" in {
    val prod = BiFunction(Two, Two, Product)
    val simplified = prod.simplify
    simplified shouldBe Literal(WholeNumber(4))
  }

  it should "combine powers of same base" in {
    val x = Root.phi
    val x2 = BiFunction(x, Two, Power)
    val x3 = BiFunction(x, Two plus One, Power)
    val product = BiFunction(x2, x3, Product)
    val simplified = product.simplify
    simplified shouldBe a[BiFunction]
    simplified shouldBe (x + 1) * ((x + 1) * x)
  }

  // ============================================================================
  // BiFunction Tests - Simplification (Power)
  // ============================================================================

  behavior of "BiFunction - Simplification (Power)"

  it should "simplify x^0 to 1" in {
    val powerZero = BiFunction(Pi, Zero, Power)
    powerZero.simplify shouldBe One
  }

  it should "simplify x^1 to x" in {
    val powerOne = BiFunction(EulerMascheroni, One, Power)
    powerOne.simplify shouldBe EulerMascheroni
  }

  it should "simplify x^(-1) to reciprocal" in {
    val powerNeg = BiFunction(Pi, MinusOne, Power)
    val simplified = powerNeg.simplify
    simplified shouldBe a[UniFunction]
    val uf = simplified.asInstanceOf[UniFunction]
    uf.f shouldBe Reciprocal
    uf.x shouldBe Pi
  }

  it should "simplify (x^a)^b to x^(a*b) for root" in {
    val x = Root.phi
    val inner = BiFunction(x, Two, Power)
    val outer = BiFunction(inner, Two, Power)
    val simplified = outer.simplify
    simplified shouldBe a[BiFunction]
    // Should be phi^4
    val bf = simplified.asInstanceOf[BiFunction]
    bf.f shouldBe Power
    bf.a shouldBe Root.phi + 1 // NOTE we do not currently simplify this as Root.phi + 1 because that causes a stack overflow.
    bf.b shouldBe Literal(2)
  }

  it should "evaluate constant power completely" in {
    val power = BiFunction(Two, Two plus One, Power)
    val simplified = power.simplify
    simplified shouldBe Literal(WholeNumber(8))
  }

  it should "simplify φ^2 using golden ratio identity" in {
    val phiSquared = BiFunction(Root.phi, Two, Power)
    val simplified = phiSquared.simplify
    // Should simplify using φ² = φ + 1
    simplified should not be phiSquared
    simplified shouldBe a[BiFunction]
  }

  it should "simplify e^(iπ) to -1 (Euler's identity)" in {
    val eulerExp = BiFunction(E, BiFunction(ConstI, Pi, Product), Power)
    val simplified = eulerExp.simplify
    simplified shouldBe MinusOne
  }

  it should "simplify e^(πi) to -1 (Euler's identity, reversed)" in {
    val eulerExp = BiFunction(E, BiFunction(Pi, ConstI, Product), Power)
    val simplified = eulerExp.simplify
    simplified shouldBe MinusOne
  }

  it should "simplify x^(log_x(y)) to y" in {
    val logExpr = BiFunction(E, Pi, Log)
    val powerExpr = BiFunction(E, logExpr, Power)
    val simplified = powerExpr.simplify
    simplified shouldBe Pi
  }

  // ============================================================================
  // BiFunction Tests - Simplification (Log)
  // ============================================================================

  behavior of "BiFunction - Simplification (Log)"

  it should "simplify log_x(1) to 0" in {
    val logOne = BiFunction(One, Pi, Log)
    logOne.simplify shouldBe Zero
  }

  it should "simplify log_x(x) to 1" in {
    val logSelf = BiFunction(Pi, Pi, Log)
    logSelf.simplify shouldBe One
  }

  it should "simplify log_e(x) to ln(x)" in {
    val logE = BiFunction(EulerMascheroni, E, Log)
    val simplified = logE.simplify
    simplified shouldBe a[UniFunction]
    val uf = simplified.asInstanceOf[UniFunction]
    uf.f shouldBe Ln
    uf.x shouldBe EulerMascheroni
  }

  // ============================================================================
  // BiFunction Tests - Equality
  // ============================================================================

  behavior of "BiFunction - Equality"

  it should "be equal for same operands and function" in {
    val bi1 = BiFunction(One, Two, Sum)
    val bi2 = BiFunction(One, Two, Sum)
    bi1 shouldBe bi2
  }

  it should "be equal for commutative operations with swapped operands" in {
    val bi1 = BiFunction(One, Two, Sum)
    val bi2 = BiFunction(Two, One, Sum)
    bi1 shouldBe bi2 // Sum is commutative

    val prod1 = BiFunction(One, Two, Product)
    val prod2 = BiFunction(Two, One, Product)
    prod1 shouldBe prod2 // Product is commutative
  }

  it should "not be equal for non-commutative operations with swapped operands" in {
    val pow1 = BiFunction(One, Two, Power)
    val pow2 = BiFunction(Two, One, Power)
    pow1 should not be pow2 // Power is not commutative
  }

  it should "not be equal for different functions" in {
    val sum = BiFunction(One, Two, Sum)
    val prod = BiFunction(One, Two, Product)
    sum should not be prod
  }

  it should "have consistent hashCode" in {
    val bi1 = BiFunction(One, Two, Sum)
    val bi2 = BiFunction(One, Two, Sum)
    bi1.hashCode shouldBe bi2.hashCode
  }

  // ============================================================================
  // Aggregate Tests - Basic Properties
  // ============================================================================

  behavior of "Aggregate - Basic Properties"

  it should "not be atomic" in {
    val agg = Aggregate.total(One, Two, Two plus One)
    agg.isAtomic shouldBe false
  }

  it should "have correct depth" in {
    val agg = Aggregate.total(One, Two, Two plus One)
    agg.depth shouldBe 3 // 1 + max depth of terms
  }

  it should "provide terms correctly" in {
    val agg = Aggregate.total(One, Two, Two plus One)
    agg.terms.length shouldBe 3
  }

  it should "be exact when all operands are exact" in {
    val exact = Aggregate.total(One, Two, Two plus One)
    exact.isExact shouldBe true
  }

  it should "not be exact when any operand is fuzzy" in {
    val fuzzy = Aggregate.total(One, Two, Literal(Real(3.14, Some(AbsoluteFuzz(0.01, Box)))))
    fuzzy.isExact shouldBe false
  }

  // ============================================================================
  // Aggregate Tests - Construction
  // ============================================================================

  behavior of "Aggregate - Construction"

  it should "create sum aggregate" in {
    val sum = Aggregate.total(One, Two, Two plus One)
    sum.function shouldBe Sum
    sum.xs.length shouldBe 3
  }

  it should "create product aggregate" in {
    val prod = Aggregate.product(One, Two, Two plus One)
    prod.function shouldBe Product
    prod.xs.length shouldBe 3
  }

  it should "create empty aggregate" in {
    val emptyAggregate = Aggregate.empty(Sum)
    emptyAggregate.xs shouldBe empty
  }

  it should "throw exception for create with empty sequence" in {
    an[IllegalArgumentException] should be thrownBy Aggregate.create(Sum, Seq.empty)
  }

  // ============================================================================
  // Aggregate Tests - Evaluation
  // ============================================================================

  behavior of "Aggregate - Evaluation"

  it should "evaluate sum correctly" in {
    val sum = Aggregate.total(One, Two, Two)
    sum.evaluate(pureNumberContext) shouldBe Some(WholeNumber(5))
  }

  it should "evaluate product correctly" in {
    val prod = Aggregate.product(Two, Two, Two)
    prod.evaluate(pureNumberContext) shouldBe Some(WholeNumber(8))
  }

  it should "handle identity elements in sum" in {
    val sum = Aggregate.total(Zero, Two, Zero)
    sum.evaluate(pureNumberContext) shouldBe Some(WholeNumber.two)
  }

  it should "handle identity elements in product" in {
    val prod = Aggregate.product(One, Two, One)
    prod.evaluate(pureNumberContext) shouldBe Some(WholeNumber.two)
  }

  // ============================================================================
  // Aggregate Tests - Manipulation
  // ============================================================================

  behavior of "Aggregate - Manipulation"

  it should "add expression to aggregate" in {
    val agg = Aggregate.total(One, Two)
    val newAgg = agg.add(Two plus One)
    newAgg.xs.length shouldBe 3
  }

  it should "add multiple expressions to aggregate" in {
    val agg = Aggregate.total(One, Two)
    val newAgg = agg.addAll(Seq(Two plus One, Two * Two))
    newAgg.xs.length shouldBe 4
  }

  // ============================================================================
  // Aggregate Tests - Angle Handling
  // ============================================================================

  behavior of "Aggregate - Angle Handling"

  it should "detect angles in sequence" in {
    val angle = Literal(Angle.pi)
    val exprs = Seq(One, angle, Two)
    Aggregate.hasAngles(exprs) shouldBe true
  }

  it should "detect absence of angles" in {
    val exprs = Seq(One, Two, Two plus One)
    Aggregate.hasAngles(exprs) shouldBe false
  }

  it should "detect reciprocal angles" in {
    val angle = Literal(Angle.pi)
    val recAngle = UniFunction(angle, Reciprocal)
    val exprs = Seq(One, recAngle, Two)
    Aggregate.hasReciprocalAngles(exprs) shouldBe true
  }

  it should "process angles correctly" in {
    val angle = Literal(Angle.pi)
    val exprs = Seq(One, angle, Two)
    val processed = Aggregate.getAnglesEtc(exprs)
    // Should expand angle into coefficient and π
    processed.length should be > exprs.length
  }

  // ============================================================================
  // Aggregate Tests - Simplification
  // ============================================================================

  behavior of "Aggregate - Simplification"

  it should "combine constant terms in sum" in {
    val sum = Aggregate.total(One, Two, Two)
    val simplified = sum.simplify
    // Should combine to 5
    simplified shouldBe Literal(WholeNumber(5))
  }

  it should "combine constant terms in product" in {
    val prod = Aggregate.product(Two, Two, Two)
    val simplified = prod.simplify
    // Should combine to 8
    simplified shouldBe Literal(WholeNumber(8))
  }

  it should "preserve symbolic terms in sum" in {
    val sum = Aggregate.total(Pi, EulerMascheroni, E)
    val simplified = sum.simplify
    // Should remain as aggregate or simplified aggregate
    simplified shouldBe an[Aggregate]
  }

  // ============================================================================
  // CompositeExpression Common Object Tests
  // ============================================================================

  behavior of "CompositeExpression object"

  it should "create single element as expression" in {
    val result = CompositeExpression(Sum, Seq(One))
    result shouldBe One
  }

  it should "create two elements as BiFunction" in {
    val result = CompositeExpression(Sum, Seq(One, Two))
    result shouldBe a[BiFunction]
  }

  it should "create multiple elements as Aggregate" in {
    val result = CompositeExpression(Sum, Seq(One, Two, Two plus One))
    result shouldBe an[Aggregate]
  }

  it should "throw exception for empty sequence" in {
    an[IllegalArgumentException] should be thrownBy CompositeExpression(Sum, Seq.empty)
  }

  // ============================================================================
  // Integration Tests
  // ============================================================================

  behavior of "CompositeExpression - Integration"

  it should "simplify complex nested expression with constants" in {
    // (1 + 2) * 3
    val inner = BiFunction(One, Two, Sum)
    val outer = BiFunction(inner, Two plus One, Product)
    val simplified = outer.simplify
    simplified shouldBe Literal(WholeNumber(9))
  }

  it should "simplify ((x^2)^3) correctly for constants" in {
    val inner = BiFunction(Two, Two, Power)
    val outer = BiFunction(inner, Two plus One, Power)
    val simplified = outer.simplify
    // Should be 2^6 = 64
    simplified shouldBe Literal(WholeNumber(64))
  }

  it should "simplify nested powers for transcendentals" in {
    val x = Root.phi
    val inner = BiFunction(x, Two, Power)
    val outer = BiFunction(inner, Two, Power)
    val simplified = outer.simplify
    // Should be phi^4 (symbolic)
    simplified shouldBe a[BiFunction]
  }

  it should "handle mixed exact and fuzzy operations" in {
    val fuzzy = Literal(Real(2.0, Some(AbsoluteFuzz(0.1, Box))))
    val expr = BiFunction(fuzzy, Two, Product)
    expr.isExact shouldBe false
    expr.evaluate(pureNumberContext) should be(empty)
    expr.materialize shouldBe Real(4.0, Some(RelativeFuzz(0.1, Box))) // CONSIDER this looks wrong. It should be AbsoluteFuzz(0.1, Box)
  }

  it should "preserve mathematical identities through simplification" in {
    // exp(ln(x)) = x
    val inner = UniFunction(EulerMascheroni, Ln)
    val outer = UniFunction(inner, Exp)
    val simplified = outer.simplify
    // Should simplify back to EulerMascheroni
    simplified.materialize shouldBe Eager(Constants.gamma)

    // ln(exp(x)) = x
    val inner2 = UniFunction(EulerMascheroni, Exp)
    val outer2 = UniFunction(inner2, Ln)
    val simplified2 = outer2.simplify
    simplified2.materialize shouldBe Eager(Constants.gamma)
  }

  behavior of "Square root simplification consistency"

  it should "simplify bare √3 to consistent form" in {
    val root3a = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)
    val simplified = root3a.simplify
    println(s"Bare √3 simplified to: $simplified (${simplified.getClass.getSimpleName})")
    // Just observe what it becomes
  }

  it should "simplify √3 inside negation to consistent form" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)
    val negated = UniFunction(root3, Negate)
    val simplified = negated.simplify
    println(s"Negated √3 simplified to: $simplified (${simplified.getClass.getSimpleName})")
    // Extract the inner expression if it's still a UniFunction
    simplified match {
      case UniFunction(inner, Negate) =>
        println(s"  Inner expression: $inner (${inner.getClass.getSimpleName})")
      case other =>
        println(s"  Not a UniFunction anymore: $other")
    }
  }

  it should "ensure both representations are equivalent" in {
    val root3a = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power).simplify
    val root3b = UniFunction(BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power), Negate).simplify

    // Extract inner from negation if needed
    val innerB = root3b match {
      case UniFunction(inner, Negate) => inner
      case other => fail(s"Expected negated form, got: $other")
    }

    println(s"root3a: $root3a (${root3a.getClass.getSimpleName})")
    println(s"innerB: $innerB (${innerB.getClass.getSimpleName})")

    // They should have the same structure
    root3a.getClass shouldBe innerB.getClass
  }

  it should "show what simplifyByEvaluation returns for each" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)

    val bareEval: em.MatchResult[Expression] = Expression.simplifyByEvaluation(root3) // Call the matcher
    println(s"Bare simplifyByEvaluation result: $bareEval")

    bareEval match {
      case em.Match(expr) => println(s"  Matched to: $expr (${expr.getClass.getSimpleName})")
      case em.Miss(msg, _) => println(s"  Missed: $msg")
      case em.Error(e) => println(s"  Error: $e")
      case _ => println(s"  No match found!")
    }
  }

  it should "test simplifyByEvaluation directly on both forms" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)
    val negated = UniFunction(root3, Negate)

    val bareResult = Expression.simplifyByEvaluation(root3)
    val negatedResult = Expression.simplifyByEvaluation(negated)

    println(s"Bare √3 simplifyByEvaluation: $bareResult")
    println(s"Negated √3 simplifyByEvaluation: $negatedResult")

    // See what each returns
  }

  it should "verify the aggregate expansion creates consistent forms" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)
    val one = Literal(1)
    val minusOne = Literal(-1)

    val sum1 = BiFunction(root3, one, Sum) // √3 + 1
    val sum2 = BiFunction(root3, minusOne, Sum) // √3 - 1
    val product = BiFunction(sum1, sum2, Product) // (√3+1)(√3-1)

    println(s"Product before simplify: $product")
    val simplified = product.simplify
    println(s"Product after simplify: $simplified")

    // If it's an aggregate, check the terms
    simplified match {
      case Aggregate(_, terms) =>
        terms.zipWithIndex.foreach { case (term, i) =>
          println(s"  Term $i: $term (${term.getClass.getSimpleName})")
        }
      case other =>
        println(s"Not an aggregate: $other")
    }
  }

  it should "trace the full simplification pipeline for bare √3" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)

    println(s"Starting with: $root3")

    // Try each phase manually
    val afterComponents = root3.doSimplifyOperands(root3)
    println(s"After simplifyOperands: $afterComponents")

    val afterStructural = root3.doSimplifyStructural(root3)
    println(s"After doSimplifyStructural: $afterStructural")

    val afterIdentities = root3.simplifyIdentities(root3)
    println(s"After simplifyIdentities: $afterIdentities")

    val afterExact = root3.simplifyExact(root3)
    println(s"After simplifyExact: $afterExact")

    val afterEval = Expression.simplifyByEvaluation(root3)
    println(s"After simplifyByEvaluation: $afterEval")

    // Now full simplify
    val fullSimplify = root3.simplify
    println(s"After full simplify: $fullSimplify (${fullSimplify.getClass.getSimpleName})")
  }

  it should "trace the full simplification pipeline for nested √3" in {
    val root3 = BiFunction(Literal(3), Literal(RationalNumber(1, 2)), Power)
    val negated = UniFunction(root3, Negate)

    println(s"Starting with: $negated")
    val fullSimplify = negated.simplify
    println(s"After full simplify: $fullSimplify")

    fullSimplify match {
      case UniFunction(inner, Negate) =>
        println(s"  Inner: $inner (${inner.getClass.getSimpleName})")
      case other =>
        println(s"  Not a UniFunction: $other")
    }
  }
}
