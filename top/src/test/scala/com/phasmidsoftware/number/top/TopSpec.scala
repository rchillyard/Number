/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.matchers.*
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.Angle.𝛑
import com.phasmidsoftware.number.algebra.Real.∞
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Number.{piBy2, root2}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.infinity
import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box, ComplexPolar, Constants, Gaussian}
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers.shouldBe
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
  * The operations are tested against specific matching rules for common scenarios in symbolic lazymath expressions,
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

  behavior of "Noop"
  it should "return Noop" in {
    lazymath" " should matchPattern { case Noop(_) => }
  }
  it should "throw exception" in {
    an[Exception] shouldBe thrownBy(math" ")
  }
  it should "return None" in {
    mathOpt" " shouldBe None
  }

  behavior of "simplify"
  it should "cancel multiplication and division with simplify 1" in {
    lazymath"2𝛑*1/2" shouldBe ConstPi
    math"2𝛑*1/2" shouldBe 𝛑
  }
  it should "show that lazy evaluation sometimes works even when you don't use it (a2)" in {
    lazymath"√7 ^ 2" shouldBe Literal(WholeNumber(7), Some("7"))
    math"√7 ^ 2" shouldBe WholeNumber(7)
  }
  it should "properly simplify 1 * (root3 / root3 * 3)" in {
    lazymath"√3 / √3 * 3" shouldBe Literal(WholeNumber(3), Some("3"))
    math"√3 / √3 * 3" shouldBe WholeNumber(3)
  }
  it should "distributeProductSum b 0" in {
    lazymath"(2 + 1) * (3 + \frac{1}{2})" shouldBe Literal(RationalNumber(21, 2), Some("10.5"))
    math"(2 + 1) * (3 + \frac{1}{2})" shouldBe Eager(r"21/2")
  }
  it should "distributeProductSum b 1" in {
    lazymath"(2.00 + 1) * (3.00 + ½)" shouldBe Literal(RationalNumber(21, 2), Some("10.5"))
    math"(2.00 + 1) * (3.00 + ½)" shouldBe Eager(r"21/2")
  }
  it should "distributeProductSum b 2" in {
    lazymath"(2.005 + 1) * (2.995 + ½)" shouldBe BiFunction(Literal(Real(3.005, Some(AbsoluteFuzz(5.0E-4, Box))), Some("3.0050[5]")), Literal(Real(3.495, Some(AbsoluteFuzz(5.0E-4, Box))), Some("3.4950[5]")), Product)
    math"(2.005 + 1) * (2.995 + ½)" should ===(Real(10.502475, Some(AbsoluteFuzz(0.012835619415020195, Gaussian))))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    val exp = """\sqrt{3}^2"""
    lazymath"$exp" shouldBe Expression(3)
    math"$exp" shouldBe Eager(3)
  }
  it should "cancel addition and subtraction (a)" in {
    lazymath"""\pi+3-3""" shouldBe ConstPi
    math"""\pi+3-3""" shouldBe Valuable.pi
  }
  it should "use multiply instead of addition" in {
    puremath"\pi+𝛑" shouldBe BiFunction(ConstPi, ConstPi, Sum)
    lazymath"\pi+𝛑" shouldBe Literal(Angle(WholeNumber(0)), Some("0𝛑"))
    math"\pi+𝛑" shouldBe Angle.zero
  }
  it should "work for Negate" in {
    lazymath"-1" shouldBe MinusOne
    lazymath"-1*1" shouldBe MinusOne
    math"-1" shouldBe Valuable.minusOne
    math"-1*1" shouldBe Valuable.minusOne
  }
  it should "work for Negate Negate" in {
    lazymath"-(-1)" shouldBe One
    lazymath"-(-𝛑)" shouldBe ConstPi
    math"-(-1)" shouldBe Valuable.one
    math"-(-𝛑)" shouldBe 𝛑
  }
  it should "work for Reciprocal" in {
    lazymath"1/2" shouldBe Literal(half)
    lazymath"\rec(2)" shouldBe Literal(half)
    math"1/2" shouldBe half
    math"\rec(2)" shouldBe half
  }
  it should "work for Reciprocal Reciprocal" in {
    lazymath"\rec{\rec{𝛑}}" shouldBe ConstPi
    math"\rec{\rec{𝛑}}" shouldBe 𝛑
  }
  it should "work for Negate Zero" in {
    val e = Zero.materialize
    lazymath"-${e.render}" shouldBe Zero
    math"-${e.render}" shouldBe Number.zero
  }
  it should "work for Reciprocal Zero" in {
    val e = Zero.materialize
    lazymath"\rec{${e.render}}" shouldBe Infinity
  }
  it should "work for Reciprocal One" in {
    val x = expr.UniFunction(One, Reciprocal)
    lazymath"1/1" shouldBe One
  }
  it should "work for Reciprocal Two" in {
    lazymath"1/2" shouldBe Literal(half)
  }
  it should "work for Exp Infinity" in {
    lazymath"\e^∞" shouldBe Infinity
    // CONSIDER should this be Real.∞ instead?
    math"\e^∞" shouldBe RationalNumber(Rational.infinity)
  }
  it should "work for Exp neg Infinity" in {
    // TODO allow Negative Infinity to be used in expressions.
    val x = expr.UniFunction(Expression(infinity.negate), Exp).simplify
    x shouldBe Zero
  }
  it should "work for Exp Zero" in {
    lazymath"\e^0" shouldBe One
    math"\e^0" shouldBe Valuable.one
  }
  it should "work for Exp One" in {
    lazymath"\e^1" shouldBe ConstE
    math"\e^1" shouldBe Valuable.e
  }
  it should "work for Ln Zero" in {
    // TODO make the following line work
//    lazymath"\ln(0)" shouldBe UniFunction(Infinity, Negate)
    math"\ln(0)" shouldBe Valuable.negInfinity
  }
  it should "work for Ln One" in {
    lazymath"\ln(1)" shouldBe Zero
    math"\ln(1)" shouldBe Valuable.zero
  }
  it should "work for Ln e" in {
    lazymath"\ln{\e}" shouldBe One
    math"\ln{\e}" shouldBe Valuable.one
  }
  it should "work for Sine 0, etc." in {
    lazymath"\sin(0)" shouldBe Zero
    math"\sin(0)" shouldBe Valuable.zero
    lazymath"\sin(\pi)" shouldBe Zero
    math"\sin(\pi)" shouldBe Valuable.zero
    lazymath"\cos(𝛑/2)" shouldBe Zero
    math"\cos(𝛑/2)" shouldBe Valuable.zero
  }
  it should "work for Sine pi/2, etc." in {
    lazymath"\sin(𝛑/2)" shouldBe One
    math"\sin(𝛑/2)" shouldBe Valuable.one
    lazymath"\cos(0)" shouldBe One
    math"\cos(0)" shouldBe Valuable.one
  }
  it should "work for Cosine pi" in {
    lazymath"\cos(𝛑)" shouldBe MinusOne
    math"\cos(𝛑)" shouldBe Valuable.minusOne
  }
  it should "cancel multiplication and division with simplify 2" in {
    lazymath"𝛑*2/2" shouldBe ConstPi
    math"𝛑*2/2" shouldBe 𝛑
    lazymath"𝛑/2*2" shouldBe ConstPi
    math"𝛑/2*2" shouldBe 𝛑
  }
  it should "simplify sqrt(7)∧2" in {
    puremath"√7 ∧ 2" shouldBe BiFunction(BiFunction(Literal(WholeNumber(7), Some("7")), Literal(RationalNumber.half, Some("½")), Power), Two, Power)
    lazymath"√7 ∧ 2" shouldBe Literal(WholeNumber(7), Some("7"))
    math"√7 ∧ 2" shouldBe Eager(7)
    math"\sqrt{7 ∧ 2}" shouldBe Eager(7)
  }
  it should "cancel 1 + -1" in {
    (lazymath"1" :+ lazymath"-1").simplify shouldBe Zero
    math"1-1" shouldBe Valuable.zero
  }
  it should "cancel 2 * 1/2 (a)" in {
    (∅ * 2 * 1 / 2).simplify shouldBe One
    lazymath"2 * 1/2" shouldBe One
    math"2 * 1/2" shouldBe Valuable.one
  }
  it should "work for multi-levels 1" in {
    math"(1+3-3)*2/4" shouldBe Valuable.half
  }
  it should "work for multi-levels 2" in {
    math"(1+\e-\e)*(\pi/4)" shouldBe Angle.pi * Rational.quarter
  }
  it should "simplify binary expression 3" in {
    math"√2 + -1*√2" shouldBe Valuable.zero
  }
  it should "simplify aggregate 4a" in {
    math"(√3+1)*(√3-1)" shouldBe Valuable.two
  }
  it should "simplify aggregate 4b" in {
    math"(√3 + 1)*(1 - √3)" shouldBe WholeNumber(-2)
  }
}