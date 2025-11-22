/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.matchers.*
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Number.{piBy2, root2}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.infinity
import com.phasmidsoftware.number.core.{AbsoluteFuzz, ComplexPolar, Constants, Gaussian}
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.languageFeature.implicitConversions.*

/**
  * Test suite for `ExpressionMatchers` and related functionality, extending `AnyFlatSpec` with ScalaTest matchers functionalities.
  *
  * TODO requires more simplification with removal of duplicates, etc.
  *
  * This class provides unit tests for various operations and matchers related to `Expression` objects.
  * It validates the functionality of arithmetic operations, simplifications, matchers, and custom equality logic for expressions.
  *
  * The tests involve operations such as addition, subtraction, multiplication, division, power, square root,
  * and comparison on various types of `Expression` instances, including `Literal`, `One`, `Number`, and constants like `pi` and `e`.
  *
  * The operations are tested against specific matching rules for common scenarios in symbolic math expressions,
  * ensuring the correctness of `ExpressionMatchers`, custom defined `ExpressionMatchers.ExpressionMatcher` implementations,
  * and simplification functionalities such as `matchSimpler` and `simplifyTrivial`.
  *
  * Behavior-driven tests are grouped by functionality to validate the logic and expected outcomes for:
  * - Matching values via custom expression matchers.
  * - Matching and combining rules using logical OR (`|`).
  * - Simplification logic for trivial arithmetic and symbolic cancellations.
  * - Handling edge cases like exact evaluation, reciprocal, and multi-level symbolic transformations.
  *
  * Several tests focus on validating simplification scenarios, specifically ensuring the cancellation of
  * arithmetic operations like addition/subtraction, multiplication/division, and power/exponentiation inversions.
  *
  * Helper implicits such as `ExpressionEquality` and logging mechanisms (`MatchLogger`) assist in testing and debugging the logic.
  */
class TopSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case v: Eager => a.compare(Literal(v)) == 0
      case n: core.Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case _ => false
    }
  }

  val sb = new StringBuilder
  implicit val logger: MatchLogger = MatchLogger(LogDebug, classOf[ExpressionMatchers])
  //  implicit val logger: MatchLogger = SBLogger(LogOff, sb)

  before {
    sb.clear()
  }

  after {
    //    if (sb.nonEmpty) println(sb.toString())
    //    if (sbLogger.logLevel != com.phasmidsoftware.matchers.LogOff)
    //      println("===============================\n")
  }

  val em: ExpressionMatchers = Expression.em
  val eml: ExpressionMatchers = new ExpressionMatchers() {}
  private val two: Eager = Valuable.two
  private val one: Eager = Valuable.one
  private val half: Eager = Valuable.half


  import Expression.ExpressionOps
  import Matchers.*
  import Rational.RationalHelper
  import com.phasmidsoftware.number.parsenew.ExpressionParser.*

  behavior of "simplify"
  it should "cancel multiplication and division with simplify 1" in {
    math"2𝛑*1/2".simplify shouldBe ConstPi
  }
  it should "show that lazy evaluation sometimes works even when you don't use it (a2)" in {
    math"√7 ^ 2".simplify shouldBe Expression(7)
  }
  it should "properly simplify 1 * (root3 / root3 * 3)" in {
    math"√3 / √3 * 3".materialize shouldBe WholeNumber(3)
  }
  it should "distributeProductSum b 0" in {
    math"(2 + 1) * (3 + \frac{1}{2})".materialize shouldBe Eager(r"21/2")
  }
  it should "distributeProductSum b 1" in {
    math"(2.00 + 1) * (3.00 + ½)".materialize shouldBe Eager(r"21/2")
  }
  it should "distributeProductSum b 2" in {
    math"(2.005 + 1) * (2.995 + ½)".materialize should ===(Real(10.502475, Some(AbsoluteFuzz(0.012835619415020195, Gaussian))))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x = Expression(3).sqrt
    val q = p(Product ~ x ~ x)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Eager(3)
  }
  it should "cancel addition and subtraction (a)" in {
    val x = ConstPi + 3 - 3
    val simplified = x.simplify
    simplified.evaluateAsIs shouldBe Some(Valuable.pi)
  }
  it should "use multiply instead of addition" in {
    val x = ConstPi + ConstPi
    val simplified = x.simplify
    import core.Real.RealOps
    val expected = Expression(Eager(2 * Constants.pi))
    simplified shouldBe expected
  }
  it should "work for Negate" in {
    val x = expr.UniFunction(One, Negate)
    x.simplify shouldBe MinusOne
  }
  it should "work for Negate Negate" in {
    val x = expr.UniFunction(expr.UniFunction(One, Negate), Negate)
    x.simplify shouldBe One
  }
  it should "work for Reciprocal" in {
    val x = expr.UniFunction(Two, Reciprocal)
    x.simplify shouldBe Literal(half)
  }
  it should "work for Reciprocal Reciprocal" in {
    val x = expr.UniFunction(expr.UniFunction(ConstPi, Reciprocal), Reciprocal)
    x.simplify shouldBe ConstPi
  }
  it should "work for Negate Zero" in {
    val x = expr.UniFunction(Zero, Negate).simplify
    x shouldBe Zero
  }
  it should "work for Negate MinusOne" in {
    val x = expr.UniFunction(MinusOne, Negate).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Zero" in {
    val x = expr.UniFunction(Zero, Reciprocal).simplify
    x shouldBe Expression(infinity)
  }
  it should "work for Reciprocal One" in {
    val x = expr.UniFunction(One, Reciprocal).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Two" in {
    val x = expr.UniFunction(Two, Reciprocal).simplify
    x shouldBe Literal(half)
  }
  it should "work for Exp Infinity" in {
    val x = expr.UniFunction(Expression(infinity), Exp).simplify
    x shouldBe Infinity
  }
  it should "work for Exp neg Infinity" in {
    val x = expr.UniFunction(Expression(infinity.negate), Exp).simplify
    x shouldBe Zero
  }
  it should "work for Exp Zero" in {
    val x = expr.UniFunction(Zero, Exp)
    x.simplify shouldBe One
  }
  it should "work for Exp One" in {
    val x = expr.UniFunction(One, Exp)
    x.simplify shouldBe ConstE
  }
  it should "work for Ln Zero" in {
    val x = expr.UniFunction(Zero, Ln)
    x.simplify shouldBe Expression(infinity.negate)
  }
  it should "work for Ln One" in {
    val x = expr.UniFunction(One, Ln)
    x.simplify shouldBe Zero
  }
  it should "work for Ln e" in {
    val x = expr.UniFunction(Valuable.e, Ln)
    x.simplify shouldBe One
  }
  it should "work for Sine 0" in {
    val x = expr.UniFunction(Angle.zero, Sine)
    val simplify = x.simplify
    simplify shouldBe Zero
  }
  it should "work for Sine pi/2" in {
    val x = expr.UniFunction(Angle.piBy2, Sine)
    x.simplify shouldBe One
  }
  it should "work for Sine pi" in {
    val x = expr.UniFunction(ConstPi, Sine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine 0" in {
    val x = expr.UniFunction(Angle.zero, Cosine)
    x.simplify shouldBe One
  }
  it should "work for Cosine pi/2" in {
    val x = expr.UniFunction(Literal(piBy2), Cosine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine pi" in {
    val x = expr.UniFunction(ConstPi, Cosine)
    x.simplify shouldBe MinusOne
  }
  it should "cancel multiplication and division with simplify 2" in {
    val x = Literal(Valuable.pi) * 2
    val y = One / 2
    (x * y).simplify shouldBe ConstPi
  }
  it should "simplify sqrt(7)∧2" in {
    val x: Expression = Expression(7)
    val y = x.sqrt
    val z = y ∧ 2
    val q = z.simplify
    q shouldBe Expression(7)
  }
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = z.simplify
    p should matchPattern { case Zero => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = MinusOne * x
    val z = x + y
    val p = z.simplify
    p should matchPattern { case Zero => }
  }
  it should "cancel 2 * 1/2 (a)" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val p = z.simplify
    p shouldBe Literal(RationalNumber(1))
  }
  it should "cancel 2 * 1/2 (b)" in {
    val x = Expression(2) * Expression.one
    val y = x.reciprocal
    val z = y * x
    val p = z.simplify
    p shouldBe Literal(RationalNumber(1))
  }
  it should "cancel ∧2 and sqrt for 7" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    val p = y.simplify
    p shouldBe seven
  }
  it should "show that lazy evaluation sometimes works even when you don't use it (a1)" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ two
    val simplify = y.simplify
    simplify.isExact shouldBe true
    simplify shouldBe seven
  }
  it should "cancel multiplication and division 2" in {
    val x = Literal(Valuable.pi) * 2 / 2
    x.simplify shouldBe ConstPi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Literal(Valuable.pi) / 2 * 2
    x.simplify shouldBe ConstPi
  }
  it should "cancel 1 and - -1 (a)" in {
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    z.simplify shouldBe Zero
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    z.simplify shouldBe ValueExpression(RationalNumber(1))
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    z.simplify shouldBe ValueExpression(RationalNumber(1))
  }
  it should "cancel ∧2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    y.simplify shouldBe Expression(7)
  }
  it should "cancel addition and subtraction of 3" in {
    val x = One + 3 - 3
    x.simplify shouldBe One
  }
  it should "cancel addition and subtraction of e" in {
    val y: Expression = One + ConstE
    val z = y + expr.UniFunction(ConstE, Negate)
    z.simplify shouldBe One
  }
  it should "work for multi-levels 1" in {
    val x = (One + 3 - 3) * (Two / 4)
    x.simplify shouldBe Half
  }
  it should "work for multi-levels 2" in {
    val x = (One + ConstE - ConstE) * (ConstPi / 4)
    val simpler = x.simplify
    simpler shouldBe BiFunction(ConstPi, Literal(RationalNumber(Rational.quarter), None), Product)
  }
  it should "simplify aggregate 1" in {
    val x: Expression = Aggregate(Sum, Seq(One, 3, -3))
    //val result: em.MatchResult[Field] = em.simplifier(x) map (_.materialize)
    val result: Valuable = x.simplify.materialize
    result should matchPattern { case Valuable.one => }
  }
  ignore should "simplify aggregate 1a" in {
    val target: Expression = Aggregate(Sum, Seq(ConstPi, -ConstPi))
    val result = target.simplify.materialize
    result shouldBe Angle.zero
    //    val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    //    result match {n
    //      case em.Match(x: Field) =>
    //        convertToNumber(x) shouldBe expected
    //      case _ => fail("expected a Field")
    //    }
  }
  ignore should "simplify aggregate 2a" in {
    val target: Expression = Aggregate(Sum, Seq(ConstPi, -ConstPi))
    val simplify = target.simplify
    val result: Valuable = simplify.materialize
    result shouldBe Angle.zero
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    //    result match {
    //      case em.Match(x: Field) =>
    //        convertToNumber(x).isZero shouldBe true
    //      case _ => fail("expected a Field")
    //    }
  }
  ignore should "simplify aggregate 2b" in {
    // NOTE: this does not create a Aggregate but instead creates a BiFunction.
    val target: Expression = CompositeExpression.create(Sum, Valuable.pi, Angle.negPi)
    //val expected = ExactNumber(0, Radian) // Ideally, the result should equal this but for now, we only test isZero.
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    val result = target.simplify.materialize
    result shouldBe Angle.zero
    //    result match {
    //      case em.Match(x: Field) =>
    //        convertToNumber(x).isZero shouldBe true
    //      case _ => fail("expected a Field")
    //    }
  }
  it should "simplify binary expression 3" in {
    val target: Expression = BiFunction(Literal(root2), Literal(root2) * MinusOne, Sum)
    //val value1 = em.simplifier(target)
    val value1 = target.simplify
    value1 shouldBe Zero

    val result = value1.materialize
    result shouldBe Valuable.zero
    //    val result = value1 map (_.materialize)
    //    result match {
    //      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
    //      case x => fail(s"expected a Match(Field) but got $x")
    //    }
  }
  // FIXME this became an infinite loop when we added support for Root in algebra.
  ignore should "simplify aggregate 3a" in {
    val target: Expression = Aggregate(Sum, Seq(Literal(root2), Literal(root2) * Valuable.minusOne))
    //val value1 = em.simplifier(target)
    //val result = value1 map (_.materialize)
    val result = target.simplify.materialize
    result shouldBe Valuable.zero
    //    result match {
    //      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
    //      case x => fail(s"expected a Match(Field) but got $x")
    //    }
  }
  // FIXME infinite loop
  ignore should "simplify aggregate 4a" in {
    val target: Expression = Aggregate(Sum, Seq(One, ConstE, expr.UniFunction(ConstE, Negate)))
    //val result: em.MatchResult[Expression] = em.simplifier(target)
    val result = target.simplify
    result shouldBe One
  }
  // Was Issue #88 but that was fixed a while ago.
  it should "simplify aggregate 4b" in {
    val root3 = Expression(3).sqrt
    val root3PlusOne = root3 plus Expression.one
    val root3MinusOne = root3 plus Expression(Valuable.minusOne)
    val expression = root3PlusOne * root3MinusOne
    val result = expression.simplify
    //val result = em.simplifier(expression)
    result shouldBe Two
  }

  behavior of "materialize"
  it should "show ∧2 and sqrt for illustrative purposes (a)" in {
    val y: Expression = ValueExpression(7).sqrt ∧ Valuable.two
    y.materialize shouldBe Eager(7)
  }
  ignore should "evaluate E * 2" in {
    (Literal(Valuable.e) * 2).materialize.toString shouldBe "Real(5.43656365691809,Some(AbsoluteFuzz(5.086985018510689E-14,Box)))"
  }

  // FIXME infinite loop
  ignore should "work for Aggregate product 2" in {
    val target: CompositeExpression = Aggregate.total(Two * ConstPi, MinusOne * ConstPi)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = target.simplify
    //The expectation below is confused
    //result shouldBe Aggregate(Product, Seq(ConstPi))
    result shouldBe ConstPi
  }

  it should "work for square of square root" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    y.simplify shouldBe seven
  }
  it should "work for products" in {
    val x: Expression = 7 * 2
    val y = x * 3
    y.simplify shouldBe Expression(42)
  }
  it should "work for sums" in {
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    y.simplify shouldBe Expression(12)
  }
  it should "gather powers of 2 and * 1/2" in {
    val x: Expression = 7
    val y = x.sqrt
    val z = y ∧ 2
    z.simplify shouldBe Expression(7)
  }
  it should "simplify √3 * -1 as -√3" in {
    val root3 = Expression(3).sqrt
    val expression = BiFunction(root3, MinusOne, Product)
    expression.simplify shouldBe expr.UniFunction(root3, Negate)
  }
  it should "simplify √3 * 1 as √3" in {
    val root3 = Expression(3).sqrt
    val expression = BiFunction(root3, One, Product)
    expression.simplify shouldBe root3
  }
  it should "simplify (√3 + 1)(√3 - 1) as 2 exactly" in {
    val root3 = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 - 1)
    val z: Expression = x.simplify
    z shouldBe Expression(2)
  }
  it should "evaluate (√3 + 1)(√3 + -1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 + MinusOne)
    x.simplify shouldBe Expression(2)
  }
  it should "biFunctionSimplifier on (1 + -3)" in {
    //val p: em.ExpressionTransformer = em.biFunctionSimplifier
    val r: BiFunction = BiFunction(One, -3, Sum)
    val simplify = r.simplify
    simplify shouldBe Expression(-2)
  }
  it should "biFunctionSimplifier on (1 + √3)(1 - √3)" in {
    val x = Expression(3).sqrt
    val y = -x
    val a = BiFunction(One, x, Sum)
    val b = BiFunction(One, y, Sum)
    val r = BiFunction(a, b, Product)
    val simplify = r.simplify
    simplify shouldBe Expression(-2)
  }
  it should "properly simplify 1 + root3 - root3 + 0" in {
    val z: Expression = Expression(3).sqrt
    val x = z plus -z + Zero
    val r = One + x
    val simplified = r.simplify
    simplified shouldBe One
  }
  it should "properly simplify (1 + root3) + (zero - root3)" in {
    val root3 = Expression(3).sqrt
    val x: Expression = One + root3
    val z = Zero - root3
    val expression = x + z
    val simplified = expression.simplify
    simplified shouldBe One
  }
  it should "properly simplify (1 * root3) * (3 / root3)" in {
    val p = Expression.matchSimpler
    val root3 = Expression(3).sqrt
    val x: Expression = One * root3
    val z = Expression(3) * root3.reciprocal
    (x * z).simplify shouldBe Expression(3)
  }

  it should "simplify E" in {
    import UniFunction.*
    val r = (Exp ~ One).simplify shouldBe ConstE
  }
  it should "simplify ln(E)" in {
    import UniFunction.*
    val r = (Ln ~ ConstE).simplify shouldBe One
  }
  it should "simplify ln(1)" in {
    import UniFunction.*
    val r = (Ln ~ One).simplify shouldBe Zero
  }
  it should "simplify ln(-1)" in {
    import UniFunction.*
    val r = (Ln ~ MinusOne).simplify shouldBe Literal(Eager(ComplexPolar(core.Number.pi, core.Number.piBy2.makeNegative, 1)))
  }
  it should "work for √3 * √3" in {
    import BiFunction.*
    val e1: BiFunction = BiFunction(Expression(3), Expression(Rational.half), Power)
    val e2: BiFunction = BiFunction(Expression(3), Expression(Rational.half), Power)
    (Product ~ e1 ~ e2).simplify shouldBe Expression(3)
  }
  it should "work for √3 + -√3" in {
    val root3: Expression = Expression(3).sqrt
    val e1 = BiFunction(root3, One, Product)
    val e2 = BiFunction(root3, MinusOne, Product)
    (Sum ~ e1 ~ e2).simplify shouldBe Zero
  }
  ignore should "work for (π + 1) * (π - 1)" in {
    val e1 = BiFunction(ConstPi, One, Sum)
    val e2 = BiFunction(Literal(Valuable.pi), MinusOne, Sum)
    val result = (Product ~ e1 ~ e2).simplify shouldBe BiFunction(BiFunction(ConstPi, Two, Power), MinusOne, Sum)
  }

  behavior of "various"
  ignore should "distributeProductSum c" in {
    val x = Eager("2.00*")
    val y = Eager("3.00*")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    val z = a * b
    val eo = Expression.parse("( ( 3.00* + 0.5 ) + ( 2.00* * ( 3.00* + 0.5 ) ) )")
    eo map (e => {
      e.simplify shouldBe z
    })
  }

  private lazy val root3Expression = Literal(Eager(Constants.root3))

}