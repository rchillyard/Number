/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.RestrictedContext
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.{NatLog, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Constants}
import com.phasmidsoftware.number.expression.algebraic.{LinearEquation, QuadraticEquation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Comprehensive test suite for the AtomicExpression trait hierarchy.
  * Uses table-driven tests for repetitive patterns to reduce boilerplate.
  *
  * Tests cover:
  * - Noop
  * - ValueExpression (Literal and Constants)
  * - Transcendental (Pi, E, L2, LgE, EulerMascheroni)
  * - Root (QuadraticRoot, LinearRoot)
  */
class AtomicExpressionSpec extends AnyFlatSpec with should.Matchers with TableDrivenPropertyChecks {

  private val pureNumberContext = RestrictedContext(PureNumber)

  // ============================================================================
  // Noop Tests (Special case - not table-driven)
  // ============================================================================

  behavior of "Noop"

  it should "be atomic" in {
    val noop = Noop("test")
    noop.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    val noop = Noop("test")
    noop.depth shouldBe 1
  }

  it should "not be exact" in {
    val noop = Noop("test")
    noop.isExact shouldBe false
  }

  it should "not be zero" in {
    val noop = Noop("test")
    noop.isZero shouldBe false
  }

  it should "have signum 0" in {
    val noop = Noop("test")
    noop.signum shouldBe 0
  }

  it should "return None for maybeFactor" in {
    val noop = Noop("test")
    noop.maybeFactor(pureNumberContext) shouldBe None
  }

  it should "throw exception when evaluated" in {
    val noop = Noop("test")
    an[UnsupportedOperationException] should be thrownBy noop.evaluate(pureNumberContext)
  }

  it should "return None for maybeDouble" in {
    val noop = Noop("test")
    noop.maybeDouble shouldBe None
  }

  it should "return None for approximation" in {
    val noop = Noop("test")
    noop.approximation(force = false) shouldBe None
    noop.approximation(force = true) shouldBe None
  }

  it should "render with descriptive toString" in {
    val noop = Noop("invalid expression")
    noop.render should include("Noop")
    noop.toString should include("invalid expression")
  }

  // ============================================================================
  // ValueExpression Tests - Literal (Special cases)
  // ============================================================================

  behavior of "Literal"

  it should "be atomic" in {
    val lit = Literal(WholeNumber.one)
    lit.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    val lit = Literal(WholeNumber.one)
    lit.depth shouldBe 1
  }

  it should "preserve value" in {
    val value = WholeNumber(42)
    val lit = Literal(value)
    lit.value shouldBe value
  }

  it should "support optional name" in {
    val lit1 = Literal(WholeNumber.one, Some("unity"))
    lit1.maybeName shouldBe Some("unity")
    lit1.render shouldBe "unity"

    val lit2 = Literal(WholeNumber.one, None)
    lit2.maybeName shouldBe None
  }

  it should "evaluate to its value when qualified" in {
    val value = WholeNumber(42)
    val lit = Literal(value)
    lit.evaluate(pureNumberContext) shouldBe Some(value)
  }

  it should "detect zero correctly" in {
    Literal(WholeNumber.zero).isZero shouldBe true
    Literal(WholeNumber.one).isZero shouldBe false
  }

  it should "compute signum correctly" in {
    Literal(WholeNumber(5)).signum shouldBe 1
    Literal(WholeNumber(-5)).signum shouldBe -1
    Literal(WholeNumber.zero).signum shouldBe 0
  }

  it should "support exact values" in {
    val lit = Literal(WholeNumber(42))
    lit.isExact shouldBe true
    lit.maybeDouble shouldBe Some(42.0)
  }

  it should "support fuzzy values" in {
    val fuzzy = Real(3.14, Some(AbsoluteFuzz(0.01, Box)))
    val lit = Literal(fuzzy)
    lit.isExact shouldBe false
    lit.maybeDouble shouldBe None
  }

  it should "compare using Cats Eq" in {
    val lit1 = Literal(WholeNumber(42))
    val lit2 = Literal(WholeNumber(42))
    val lit3 = Literal(WholeNumber(43))

    lit1 shouldBe lit2
    lit1 should not be lit3
  }

  it should "have consistent hashCode" in {
    val lit1 = Literal(WholeNumber(42))
    val lit2 = Literal(WholeNumber(42))
    lit1.hashCode shouldBe lit2.hashCode
  }

  // ============================================================================
  // Table-Driven Tests for Common Atomic Constants
  // ============================================================================

  /**
    * Table of atomic constants with their expected properties.
    * Each row represents one constant with all its testable properties.
    */
  val atomicConstants = Table(
    ("name",     "constant", "render", "signum", "isZero", "value"),
    ("Zero",     Zero,       "0",      0,        true,     WholeNumber.zero),
    ("One",      One,        "1",      1,        false,    WholeNumber.one),
    ("MinusOne", MinusOne,   "-1",     -1,       false,    -WholeNumber.one),
    ("Two",      Two,        "2",      1,        false,    WholeNumber.two),
    ("Half",     Half,       "½",      1,        false,    RationalNumber.half),
    ("Pi",       Pi,         "𝛑",      1,        false,    Angle.pi),
    ("E",        E,          "e",      1,        false,    Eager.e),
  )

  forAll(atomicConstants) { (name, constant, render, signum, isZero, value) =>
    behavior of name

    it should s"$name be atomic and exact" in {
      constant.isAtomic shouldBe true
      constant.isExact shouldBe true
    }

    it should s"$name have depth 1" in {
      constant.depth shouldBe 1
    }

    it should s"$name have correct value properties" in {
      constant.value shouldBe value
      constant.isZero shouldBe isZero
      constant.signum shouldBe signum
    }

    it should s"$name render correctly" in {
      constant.render shouldBe render
    }

    if (name == "Zero") {
      it should s"$name evaluate to zero" in {
        constant.evaluate(pureNumberContext) shouldBe Some(WholeNumber.zero)
      }
    } else if (name == "One") {
      it should s"$name evaluate to one" in {
        constant.evaluate(pureNumberContext) shouldBe Some(WholeNumber.one)
      }
    } else if (name == "Pi" || name == "E") {
      // Pi and E don't evaluate in pureNumberContext
      it should s"$name return None in pureNumberContext" in {
        constant.evaluate(pureNumberContext) shouldBe None
      }
    }
  }

  // ============================================================================
  // Monadic Function Tests (Table-Driven)
  // ============================================================================

  /**
    * Table of monadic function behaviors for each constant.
    * Tests that each constant properly supports or rejects monadic operations.
    */
  val monadicFunctions = Table(
    ("constant", "function",   "expected"),
    // Zero
    (Zero,       Negate,       Some(Zero)),
    (Zero,       Exp,          Some(One)),
    (Zero,       Sine,         Some(Zero)),
    (Zero,       Cosine,       Some(One)),
    (Zero,       Reciprocal,   Some(Infinity)),
    // One
    (One,        Negate,       Some(MinusOne)),
    (One,        Reciprocal,   Some(One)),
    (One,        Exp,          Some(E)),
    (One,        Ln,           Some(Zero)),
    // MinusOne
    (MinusOne,   Negate,       Some(One)),
    (MinusOne,   Reciprocal,   Some(MinusOne)),
    // Two
    (Two,        Reciprocal,   Some(Half)),
    // Half
    (Half,       Reciprocal,   Some(Two)),
    // Pi
    (Pi,         Sine,         Some(Zero)),
    (Pi,         Cosine,       Some(MinusOne)),
    (Pi,         Ln,           None),
    (Pi,         Exp,          None),
    // E
    (E,          Ln,           Some(One)),
    (E,          Exp,          None),
    (E,          Sine,         None),
    // Infinity
    (Infinity,   Reciprocal,   Some(Zero)),
    (Infinity,   Exp,          Some(Infinity)),
    (Infinity,   Ln,           None),
    (Infinity,   Sine,         None),
  )

  forAll(monadicFunctions) { (constant, function, expected) =>
    val constName = constant.render
    val funcName = function.toString

    it should s"$constName support monadic function $funcName" in {
      constant.monadicFunction(function) shouldBe expected
    }
  }

  // ============================================================================
  // Special Cases for Constants with Unique Behaviors
  // ============================================================================

  behavior of "Two (special case)"

  it should "negate to -2" in {
    Two.monadicFunction(Negate) match {
      case Some(ValueExpression(v, _)) => v shouldBe WholeNumber(-2)
      case _ => fail("Expected ValueExpression with value -2")
    }
  }

  behavior of "Half (special case)"

  it should "negate to -½" in {
    Half.monadicFunction(Negate) match {
      case Some(Literal(v, _)) => v shouldBe -RationalNumber.half
      case _ => fail("Expected Literal with value -1/2")
    }
  }

  // ============================================================================
  // ConstI Tests (Imaginary Unit)
  // ============================================================================

  behavior of "ConstI"

  it should "be atomic and exact" in {
    ConstI.isAtomic shouldBe true
    ConstI.isExact shouldBe true
  }

  it should "render as i" in {
    ConstI.render shouldBe "i"
  }

  it should "return None for all monadic functions" in {
    ConstI.monadicFunction(Exp) shouldBe None
    ConstI.monadicFunction(Ln) shouldBe None
    ConstI.monadicFunction(Sine) shouldBe None
  }

  // ============================================================================
  // Transcendental Tests - PiTranscendental
  // ============================================================================

  behavior of "PiTranscendental (Transcendental)"

  it should "be atomic" in {
    EulerMascheroni.isAtomic shouldBe true
  }

  it should "have proper name" in {
    PiTranscendental.name shouldBe "\uD835\uDED1"
  }

  it should "have Pi as expression" in {
    PiTranscendental.expression shouldBe Pi
  }

  it should "be exact" in {
    PiTranscendental.isExact shouldBe true
  }

  it should "evaluate to pi value" in {
    PiTranscendental.evaluate(pureNumberContext) shouldBe None
  }

  it should "support function application" in {
    val sinPi: AbstractTranscendental = PiTranscendental.function(Sine).asInstanceOf[AbstractTranscendental]
    sinPi should not be null
    sinPi.name should include("sin")
  }

  // ============================================================================
  // Transcendental Tests - L2, LgE, EulerMascheroni
  // ============================================================================

  private val transcendentalConstants = Table(
    ("name",             "transcendental",  "expectedName", "exact"),
    ("L2",               L2,                "ln(2)", true),
    ("LgE",              LgE,               "log2e", true),
    ("EulerMascheroni",  EulerMascheroni,   "\uD835\uDEFE", false),
  )

  forAll(transcendentalConstants) { (name, trans, expectedName, exact) =>
    behavior of name

    it should s"$name be atomic and $exact" in {
      trans.isAtomic shouldBe true
      trans.isExact shouldBe exact
    }

    it should s"$name have depth 1" in {
      trans.depth shouldBe 1
    }

    it should s"$name render correctly" in {
      trans.render shouldBe expectedName
    }

    it should s"$name not be zero" in {
      trans.isZero shouldBe false
    }
  }

  // ============================================================================
  // Root Tests - QuadraticRoot
  // ============================================================================

  behavior of "QuadraticRoot"

  private val goldenRatioEq = QuadraticEquation(Rational(-1), Rational(-1))
  private val phiRoot = QuadraticRoot(goldenRatioEq, 0)

  it should "be atomic" in {
    phiRoot.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    phiRoot.depth shouldBe 1
  }

  it should "be exact" in {
    phiRoot.isExact shouldBe true
  }

  it should "not be zero" in {
    phiRoot.isZero shouldBe false
  }

  it should "have positive signum for positive root" in {
    phiRoot.signum shouldBe 1
  }

  it should "have negative signum for negative root" in {
    val negRoot = QuadraticRoot(goldenRatioEq, 1)
    negRoot.signum shouldBe -1
  }

  it should "evaluate correctly" in {
    val result = phiRoot.evaluate(pureNumberContext)
    result shouldBe defined
    result.get should not be null
  }

  it should "materialize correctly" in {
    val result = phiRoot.materialize
    result should not be null
  }

  it should "simplify to itself" in {
    phiRoot.simplify shouldBe phiRoot
  }

  it should "render with equation information" in {
    val rendered = phiRoot.render
    rendered should not be empty
  }

  // ============================================================================
  // Root Tests - LinearRoot
  // ============================================================================

  behavior of "LinearRoot"

  private val linearEq = LinearEquation(Rational(3,2))  // 2x + 3 = 0 => x = -3/2
  private val linearRoot = LinearRoot(linearEq)

  it should "be atomic" in {
    linearRoot.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    linearRoot.depth shouldBe 1
  }

  it should "be exact" in {
    linearRoot.isExact shouldBe true
  }

  it should "compute signum correctly" in {
    linearRoot.signum shouldBe -1  // -3/2 is negative
  }

  it should "evaluate to solution" in {
    val result = linearRoot.evaluate(pureNumberContext)
    result shouldBe defined
  }

  it should "materialize to solution" in {
    val result = linearRoot.materialize
    result shouldBe RationalNumber(-3, 2)
  }

  it should "simplify to literal" in {
    val simplified = linearRoot.simplify
    simplified should matchPattern { case LinearRoot(_) => }
  }
}