/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.top

import cats.kernel.Eq
import com.phasmidsoftware.matchers.*
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.FuzzyEq
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.eager.Angle.𝛑
import com.phasmidsoftware.number.algebra.eager.Eager.{e, half, minusOne, negInfinity, one, pi, two, zero}
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.infinity
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.{Expression, *}
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * The `TopSpec` class represents a set of specifications for testing mathematical expressions,
  * including their manipulation, evaluation, simplifications, and special mathematical edge cases.
  * 
  * NOTE that this source code is well-formed, even if your IDE suggests otherwise.
  *
  * This class extends ScalaTest's `AnyFlatSpec` for behavior-driven development (BDD) style testing
  * and integrates `Matchers` for expressive assertions. It also uses the `BeforeAndAfter` trait to
  * manage setup and teardown logic around test executions.
  *
  * Key features of `TopSpec` are:
  *
  * - Defines custom equality for `Expression` types using an implicit object `ExpressionEquality`.
  * - Facilitates logging by employing an implicit `MatchLogger`.
  * - Provides setup (`before`) and teardown (`after`) logic to clear any residual state for repeatable tests.
  * - Declares instances of `ExpressionMatchers` to verify the behavior of mathematical expressions.
  * - Contains extensive behavioral tests for verifying the correctness of specific mathematical operations.
  *
  * Major areas of focus:
  * - Mathematical evaluation for various operations, such as addition, subtraction, multiplication, division, and exponentiation.
  * - Simplification of expressions, including cancelling terms, distributing products over sums, and reducing powers.
  * - Handling mathematical constants like π, e, and handling scenarios like negative infinity, zero, and complex cases.
  * - Verifying the correctness of trigonometric functions (sin, cos) across standard angle positions.
  * - Testing edge cases such as reciprocal handling, negations, and properties of mathematical identities.
  *
  * The test suite covers lazy and eager evaluation semantics to ensure the consistency and correctness
  * of mathematical expression behaviors under different contexts.
  */
class TopSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case e: Expression =>
        Eq[Expression].eqv(a, e)
      case e: Eager =>
        Eq[Expression].eqv(a, Literal(e))
      case _ =>
        false
    }
  }


  /**
    * Implicit object providing an implementation of the `Equality` type class for the `Eager` type.
    *
    * Allows comparison between instances of `Eager` or between an `Eager` instance and another object.
    *
    * The equality logic is as follows:
    * - If both arguments are of type `Eager`, it uses an instance of `Eq[Eager]` to determine equality.
    * - For any other pair of arguments, it defaults to the standard `==` equality check.
    */
  implicit object FuzzyEagerEquality extends Equality[Eager] {
    def areEqual(a: Eager, b: Any): Boolean = (a, b) match {
      case (x: Eager, y: Eager) =>
        summon[FuzzyEq[Eager]].eqv(x, y, 0.5)
      case _ =>
        a == b
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

  import Expression.ExpressionOps
  import Rational.RationalHelper
  import com.phasmidsoftware.number.parse.ExpressionParser.*

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

  behavior of "math and lazymath"
  it should "parse and render numbers" in {
    math"$one" shouldBe one
    math"$one".render shouldBe "1"
    val sevenPercent = "7%"
    import com.phasmidsoftware.number.core.inner.Rational.RationalOps
    math"$sevenPercent" shouldBe RationalNumber(7:/100, true)()
    math"$sevenPercent".render shouldBe sevenPercent
  }
  it should "parse and render angles" in {
    math"""\pi""" shouldBe pi
    math"$pi" shouldBe pi
    math"$pi".render shouldBe "𝛑"
    val degrees180 = "180°"
    math"$degrees180" shouldBe Angle.degrees(180)
    math"$degrees180".render shouldBe degrees180
  }
  it should "cancel multiplication and division with simplify 1" in {
    lazymath"2𝛑*1/2" shouldBe Pi
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
    val value = lazymath"(2.005 + 1) * (2.995 + ½)"
    value shouldBe Literal(RationalNumber(420099, 40000))
    math"(2.005 + 1) * (2.995 + ½)" shouldBe RationalNumber(420099, 40000)
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    val exp = """\sqrt{3}^2"""
    lazymath"$exp" shouldBe Expression(3)
    math"$exp" shouldBe Eager(3)
  }
  it should "cancel addition and subtraction (a)" in {
    lazymath"""\pi+3-3""" shouldBe Pi
    math"""\pi+3-3""" shouldBe pi
  }
  it should "use multiply instead of addition" in {
    puremath"\pi+𝛑" shouldBe BiFunction(Pi, Pi, Sum)
    lazymath"\pi+𝛑" shouldBe Pi * Two
    math"\pi+𝛑" shouldBe Angle(2)
  }
  it should "work for Negate" in {
    lazymath"-1" shouldBe MinusOne
    lazymath"-1*1" shouldBe MinusOne
    math"-1" shouldBe minusOne
    math"-1*1" shouldBe minusOne
  }
  it should "work for Negate Negate" in {
    lazymath"-(-1)" shouldBe One
    lazymath"-(-𝛑)" shouldBe Pi
    math"-(-1)" shouldBe one
    math"-(-𝛑)" shouldBe 𝛑
  }
  it should "work for Reciprocal" in {
    lazymath"1/2" shouldBe Literal(half)
    lazymath"\rec(2)" shouldBe Literal(half)
    math"1/2" shouldBe half
    math"\rec(2)" shouldBe half
  }
  it should "work for Reciprocal Reciprocal" in {
    lazymath"\rec{\rec{𝛑}}" shouldBe Pi
    math"\rec{\rec{𝛑}}" shouldBe 𝛑
  }
  it should "work for Negate Zero" in {
    val e = Zero.materialize
    lazymath"-${e.render}" shouldBe Zero
    math"-${e.render}" shouldBe eager.Number.zero
  }
  it should "work for Reciprocal Zero" in {
    val e = Zero.materialize
    lazymath"\rec{${e.render}}" shouldBe Infinity
  }
  it should "work for Reciprocal One" in {
//    val x = expr.UniFunction(One, Reciprocal)
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
    val exp = puremath"\e^0"
    exp shouldBe BiFunction(E, Zero, Power)
    exp.simplify shouldBe One
    lazymath"\e^0" shouldBe One
    math"\e^0" shouldBe one
  }
  it should "work for Exp One" in {
    lazymath"\e^1" shouldBe E
    math"\e^1" shouldBe e
  }
  it should "work for Ln Zero" in {
    // TODO make the following line work
//    lazymath"\ln(0)" shouldBe UniFunction(Infinity, Negate)
    math"\ln(0)" shouldBe negInfinity
  }
  it should "work for Ln One" in {
    lazymath"\ln(1)" shouldBe Zero
    math"\ln(1)" shouldBe zero
  }
  it should "work for Ln e" in {
    lazymath"\ln{\e}" shouldBe One
    math"\ln{\e}" shouldBe one
  }
  it should "work for Sine 0, etc." in {
    lazymath"\sin(0)" shouldBe Zero
    math"\sin(0)" shouldBe zero
    lazymath"\sin(\pi)" shouldBe Zero
    math"\sin(\pi)" shouldBe zero
    lazymath"\cos(𝛑/2)" shouldBe Zero
    math"\cos(𝛑/2)" shouldBe zero
  }
  it should "work for Sine pi/2, etc." in {
    lazymath"\sin(𝛑/2)" shouldBe One
    math"\sin(𝛑/2)" shouldBe one
    lazymath"\cos(0)" shouldBe One
    math"\cos(0)" shouldBe one
  }
  it should "work for Cosine pi" in {
    lazymath"\cos(𝛑)" shouldBe MinusOne
    math"\cos(𝛑)" shouldBe minusOne
  }
  it should "work for sine special cases" in {
    val expected = (Root.squareRoot(Rational(6)) + Root.rootTwo) / 4
    val actual = math"\sin(5𝛑/12)"
    actual should ===(expected.materialize)
  }
  it should "cancel multiplication and division with simplify 2" in {
    lazymath"𝛑*2/2" shouldBe Pi
    math"𝛑*2/2" shouldBe 𝛑
    lazymath"𝛑/2*2" shouldBe Pi
    math"𝛑/2*2" shouldBe 𝛑
  }
  it should "simplify sqrt(7)∧2" in {
    puremath"√7 ∧ 2" shouldBe BiFunction(BiFunction(Literal(WholeNumber(7), Some("7")), Literal(RationalNumber.half, Some("½")), Power), Two, Power)
    lazymath"√7 ∧ 2" shouldBe Literal(WholeNumber(7), Some("7"))
    math"√7 ∧ 2" shouldBe Eager(7)
    math"\sqrt{7 ∧ 2}" shouldBe Eager(7)
  }
  it should "simplify phi∧2" in {
    puremath"\phi ∧ 2" shouldBe BiFunction(Root.phi, Two, Power)
    lazymath"\phi ∧ 2" shouldBe BiFunction(Root.phi, One, Sum)
    math"\phi ∧ 2" === Real("2.618033988749895*")
  }
  it should "cancel 1 + -1" in {
    (lazymath"1" :+ lazymath"-1").simplify shouldBe Zero
    math"1-1" shouldBe zero
  }
  it should "cancel 2 * 1/2 (a)" in {
    (∅ * 2 * 1 / 2).simplify shouldBe One
    lazymath"2 * 1/2" shouldBe One
    math"2 * 1/2" shouldBe one
  }
  it should "work for multi-levels 1" in {
    math"(1+3-3)*2/4" shouldBe half
  }
  it should "work for multi-levels 2" in {
    math"(1+\e-\e)*(\pi/4)" shouldBe Angle.pi * Rational.quarter
  }
  it should "simplify binary expression 3" in {
    math"√2 + -1*√2" shouldBe zero
  }
  it should "simplify aggregate 4a" in {
    math"(√3+1)*(√3-1)" shouldBe two
  }
  it should "simplify aggregate 4b" in {
    math"(√3 + 1)*(1 - √3)" shouldBe WholeNumber(-2)
  }
  behavior of "Other stuff"
  it should "" in {
    import Rational.RationalOps
    import com.phasmidsoftware.number.expression.expr.Expression.* // For One, etc.

    val x = 1 :/ 2  // Exact rational: 1/2
    val lazyHalf = One / 2
    val half: Eager = lazyHalf.materialize
    half shouldBe RationalNumber(x)
  }

  behavior of "Expression parsing"

  it should "parse power with negative exponent (new style)" in {
    val e: Expression = puremath"2 ∧ -1"
    e shouldBe Literal(2) ∧ -1
  }

  it should "parse power with negative exponent (deprecated Expression.parse with chs)" in {
    val eo: Option[Expression] = Expression.parse("2 ∧ (1 chs)")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(2, BiFunction(1, -1, Product), Power)
  }
}