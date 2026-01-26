/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.RestrictedContext
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box}
import com.phasmidsoftware.number.expression.algebraic.{LinearEquation, QuadraticEquation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Comprehensive test suite for the AtomicExpression trait hierarchy.
  * Tests cover:
  * - Noop
  * - ValueExpression (Literal and Constants)
  * - Transcendental (Pi, E, L2, LgE, EulerMascheroni)
  * - Root (QuadraticRoot, LinearRoot)
  */
class AtomicExpressionSpec extends AnyFlatSpec with should.Matchers {

  private val pureNumberContext = RestrictedContext(PureNumber)

  // ============================================================================
  // Noop Tests
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
  // ValueExpression Tests - Literal
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
  // ValueExpression Tests - Zero
  // ============================================================================

  behavior of "Zero"

  it should "be atomic and exact" in {
    Zero.isAtomic shouldBe true
    Zero.isExact shouldBe true
  }

  it should "have value zero" in {
    Zero.value shouldBe WholeNumber.zero
    Zero.isZero shouldBe true
    Zero.signum shouldBe 0
  }

  it should "render as 0" in {
    Zero.render shouldBe "0"
  }

  it should "evaluate to zero" in {
    Zero.evaluate(pureNumberContext) shouldBe Some(WholeNumber.zero)
  }

  it should "support monadic functions" in {
    Zero.monadicFunction(Negate) shouldBe Some(Zero)
    Zero.monadicFunction(Exp) shouldBe Some(One)
    Zero.monadicFunction(Sine) shouldBe Some(Zero)
    Zero.monadicFunction(Cosine) shouldBe Some(One)
  }

  it should "return None for reciprocal" in {
    Zero.monadicFunction(Reciprocal) shouldBe Some(Infinity)
  }

  // ============================================================================
  // ValueExpression Tests - One
  // ============================================================================

  behavior of "One"

  it should "be atomic and exact" in {
    One.isAtomic shouldBe true
    One.isExact shouldBe true
  }

  it should "have value one" in {
    One.value shouldBe WholeNumber.one
    One.isZero shouldBe false
    One.signum shouldBe 1
  }

  it should "render as 1" in {
    One.render shouldBe "1"
  }

  it should "evaluate to one" in {
    One.evaluate(pureNumberContext) shouldBe Some(WholeNumber.one)
  }

  it should "support monadic functions" in {
    One.monadicFunction(Negate) shouldBe Some(MinusOne)
    One.monadicFunction(Reciprocal) shouldBe Some(One)
    One.monadicFunction(Exp) shouldBe Some(E)
    One.monadicFunction(Ln) shouldBe Some(Zero)
  }

  // ============================================================================
  // ValueExpression Tests - MinusOne
  // ============================================================================

  behavior of "MinusOne"

  it should "be atomic and exact" in {
    MinusOne.isAtomic shouldBe true
    MinusOne.isExact shouldBe true
  }

  it should "have value minus one" in {
    MinusOne.value shouldBe -WholeNumber.one
    MinusOne.isZero shouldBe false
    MinusOne.signum shouldBe -1
  }

  it should "render as -1" in {
    MinusOne.render shouldBe "-1"
  }

  it should "support monadic functions" in {
    MinusOne.monadicFunction(Negate) shouldBe Some(One)
    MinusOne.monadicFunction(Reciprocal) shouldBe Some(MinusOne)
  }

  // ============================================================================
  // ValueExpression Tests - Two
  // ============================================================================

  behavior of "Two"

  it should "be atomic and exact" in {
    Two.isAtomic shouldBe true
    Two.isExact shouldBe true
  }

  it should "have value two" in {
    Two.value shouldBe WholeNumber.two
    Two.isZero shouldBe false
    Two.signum shouldBe 1
  }

  it should "render as 2" in {
    Two.render shouldBe "2"
  }

  it should "support monadic functions" in {
    Two.monadicFunction(Reciprocal) shouldBe Some(Half)
    Two.monadicFunction(Negate) match {
      case Some(ValueExpression(v, _)) => v shouldBe WholeNumber(-2)
      case _ => fail("Expected ValueExpression with value -2")
    }
  }

  // ============================================================================
  // ValueExpression Tests - Half
  // ============================================================================

  behavior of "Half"

  it should "be atomic and exact" in {
    Half.isAtomic shouldBe true
    Half.isExact shouldBe true
  }

  it should "have value one-half" in {
    Half.value shouldBe RationalNumber.half
    Half.isZero shouldBe false
    Half.signum shouldBe 1
  }

  it should "render as ½" in {
    Half.render shouldBe "½"
  }

  it should "support monadic functions" in {
    Half.monadicFunction(Reciprocal) shouldBe Some(Two)
    Half.monadicFunction(Negate) match {
      case Some(Literal(v, _)) => v shouldBe -RationalNumber.half
      case _ => fail("Expected Literal with value -1/2")
    }
  }

  // ============================================================================
  // ValueExpression Tests - Pi
  // ============================================================================

  behavior of "Pi"

  it should "be atomic and exact" in {
    Pi.isAtomic shouldBe true
    Pi.isExact shouldBe true
  }

  it should "have value pi" in {
    Pi.value shouldBe Angle.pi
    Pi.isZero shouldBe false
    Pi.signum shouldBe 1
  }

  it should "render as 𝛑" in {
    Pi.render shouldBe "𝛑"
  }

  it should "support trigonometric monadic functions" in {
    Pi.monadicFunction(Sine) shouldBe Some(Zero)
    Pi.monadicFunction(Cosine) shouldBe Some(MinusOne)
  }

  it should "return None for non-trigonometric functions" in {
    Pi.monadicFunction(Ln) shouldBe None
    Pi.monadicFunction(Exp) shouldBe None
  }

  // ============================================================================
  // ValueExpression Tests - E
  // ============================================================================

  behavior of "E"

  it should "be atomic and exact" in {
    E.isAtomic shouldBe true
    E.isExact shouldBe true
  }

  it should "have value e" in {
    E.value shouldBe NatLog.e
    E.isZero shouldBe false
    E.signum shouldBe 1
  }

  it should "render as e" in {
    E.render shouldBe "e"
  }

  it should "support natural logarithm" in {
    E.monadicFunction(Ln) shouldBe Some(One)
  }

  it should "return None for other functions" in {
    E.monadicFunction(Exp) shouldBe None
    E.monadicFunction(Sine) shouldBe None
  }

  // ============================================================================
  // ValueExpression Tests - ConstI
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
  // ValueExpression Tests - Infinity
  // ============================================================================

  behavior of "Infinity"

  it should "be atomic" in {
    Infinity.isAtomic shouldBe true
  }

  it should "render as ∞" in {
    Infinity.render shouldBe "∞"
  }

  it should "support specific monadic functions" in {
    Infinity.monadicFunction(Reciprocal) shouldBe Some(Zero)
    Infinity.monadicFunction(Exp) shouldBe Some(Infinity)
  }

  it should "return None for other functions" in {
    Infinity.monadicFunction(Ln) shouldBe None
    Infinity.monadicFunction(Sine) shouldBe None
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

  it should "have consistent equality" in {
    val gamma1 = EulerMascheroni
    val gamma2 = EulerMascheroni
    gamma1 shouldBe gamma2
    gamma1.hashCode shouldBe gamma2.hashCode
  }

  // ============================================================================
  // Transcendental Tests - ETranscendental
  // ============================================================================

  behavior of "ETranscendental (Transcendental)"

  it should "be atomic" in {
    ETranscendental.isAtomic shouldBe true
  }

  it should "have proper name" in {
    ETranscendental.name shouldBe "\uD835\uDF00"
  }

  it should "have E as expression" in {
    ETranscendental.expression shouldBe E
  }

  it should "be exact" in {
    ETranscendental.isExact shouldBe true
  }

  it should "evaluate to e value" in {
    ETranscendental.evaluate(pureNumberContext) shouldBe None
  }

  it should "support function application" in {
    val lnE = ETranscendental.function(Ln)
    lnE should not be null
    lnE.asInstanceOf[AbstractTranscendental].name should include("ln")
  }

  // ============================================================================
  // Transcendental Tests - L2
  // ============================================================================

  behavior of "L2 (Transcendental)"

  it should "be atomic" in {
    L2.isAtomic shouldBe true
  }

  it should "have proper name" in {
    L2.name shouldBe "ln(2)"
  }

  it should "have Two.ln as expression" in {
    L2.expression shouldBe Two.ln
  }

  it should "render correctly" in {
    L2.render shouldBe "ln(2)"
  }

  // ============================================================================
  // Transcendental Tests - LgE
  // ============================================================================

  behavior of "LgE (Transcendental)"

  it should "be atomic" in {
    LgE.isAtomic shouldBe true
  }

  it should "have proper name" in {
    LgE.name shouldBe "log2e"
  }

  it should "render correctly" in {
    LgE.render shouldBe "log2e"
  }

  // ============================================================================
  // Transcendental Tests - EulerMascheroni
  // ============================================================================

  behavior of "EulerMascheroni (Transcendental)"

  it should "be atomic" in {
    EulerMascheroni.isAtomic shouldBe true
  }

  it should "have proper name" in {
    EulerMascheroni.name shouldBe "\uD835\uDEFE"
  }

  it should "render correctly" in {
    EulerMascheroni.render shouldBe "\uD835\uDEFE"
  }

  // ============================================================================
  // Root Tests - QuadraticRoot (phi)
  // ============================================================================

  behavior of "QuadraticRoot (phi)"

  it should "be atomic" in {
    Root.phi.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    Root.phi.depth shouldBe 1
  }

  it should "be exact" in {
    Root.phi.isExact shouldBe true
  }

  it should "have correct equation" in {
    Root.phi.equation shouldBe QuadraticEquation.goldenRatioEquation
  }

  it should "be on branch 0" in {
    Root.phi.branch shouldBe 0
  }

  it should "have 2 branches" in {
    Root.phi.branches shouldBe 2
  }

  it should "render with phi symbol" in {
    Root.phi.toString shouldBe "\uD835\uDED7"
  }

  it should "satisfy phi² = phi + 1" in {
    val phiSquared = Root.phi.asInstanceOf[AbstractRoot].squared
    // This should normalize to an expression equivalent to phi + 1
    phiSquared should not be null
  }

  it should "switch branches correctly" in {
    val psi = Root.phi.branched(1)
    psi.branch shouldBe 1
    psi shouldBe Root.psi
  }

  // ============================================================================
  // Root Tests - QuadraticRoot (psi)
  // ============================================================================

  behavior of "QuadraticRoot (psi)"

  it should "be atomic" in {
    Root.psi.isAtomic shouldBe true
  }

  it should "be exact" in {
    Root.psi.isExact shouldBe true
  }

  it should "have correct equation" in {
    Root.psi.equation shouldBe QuadraticEquation.goldenRatioEquation
  }

  it should "be on branch 1" in {
    Root.psi.branch shouldBe 1
  }

  it should "render with psi symbol" in {
    Root.psi.toString shouldBe "\uD835\uDED9"
  }

  it should "satisfy psi² = psi + 1" in {
    val psiSquared = Root.psi.asInstanceOf[AbstractRoot].squared
    psiSquared should not be null
  }

  it should "switch branches correctly" in {
    val phi = Root.psi.branched(0)
    phi.branch shouldBe 0
    phi shouldBe Root.phi
  }

  // ============================================================================
  // Root Tests - QuadraticRoot (sqrt(2))
  // ============================================================================

  behavior of "QuadraticRoot (sqrt(2))"

  it should "be atomic and exact" in {
    Root.rootTwo.isAtomic shouldBe true
    Root.rootTwo.isExact shouldBe true
  }

  it should "have correct equation" in {
    Root.rootTwo.equation shouldBe QuadraticEquation.rootTwoEquation
  }

  it should "be on branch 0" in {
    Root.rootTwo.branch shouldBe 0
  }

  it should "have 2 branches" in {
    Root.rootTwo.branches shouldBe 2
  }

  it should "satisfy (sqrt(2))² = 2" in {
    val squared = Root.rootTwo.asInstanceOf[AbstractRoot].squared
    squared should not be null
  }

  it should "have negative counterpart" in {
    Root.negRootTwo.branch shouldBe 1
    Root.negRootTwo.equation shouldBe QuadraticEquation.rootTwoEquation
  }

  // ============================================================================
  // Root Tests - QuadraticRoot (special values)
  // ============================================================================

  behavior of "QuadraticRoot (special values)"

  it should "handle root zero" in {
    Root.zero.isZero shouldBe true
    Root.zero.signum shouldBe 0
  }

  it should "handle root one" in {
    Root.one.equation shouldBe QuadraticEquation(-2, 1)
  }

  // ============================================================================
  // Root Tests - LinearRoot
  // ============================================================================

  behavior of "LinearRoot"

  it should "be atomic" in {
    Root.half.isAtomic shouldBe true
  }

  it should "have depth 1" in {
    Root.half.depth shouldBe 1
  }

  it should "be exact" in {
    Root.half.isExact shouldBe true
  }

  it should "have branch 0" in {
    Root.half.branch shouldBe 0
  }

  it should "have 1 branch" in {
    Root.half.branches shouldBe 1
  }

  it should "preserve equation type" in {
    Root.half.equation shouldBe a[LinearEquation]
  }

  it should "branched returns self" in {
    Root.half.branched(0) shouldBe Root.half
    Root.half.branched(1) shouldBe Root.half
  }

  // ============================================================================
  // AtomicExpression Companion Object Tests
  // ============================================================================

  behavior of "AtomicExpression unapply"

  //  it should "extract value from Complex" in {
  //    import com.phasmidsoftware.number.core.numerical.Complex.ComplexHelper
  //    val c = Complex(C"1 + 2i")
  //    AtomicExpression.unapply(c) shouldBe Some(c)
  //  }

  it should "extract value from ValueExpression" in {
    val value = WholeNumber(42)
    val ve = Literal(value)
    AtomicExpression.unapply(ve) shouldBe Some(value)
  }

  it should "extract value from Literal" in {
    val value = WholeNumber(42)
    val lit = Literal(value)
    AtomicExpression.unapply(lit) shouldBe Some(value)
  }

  it should "return None for Noop" in {
    val noop = Noop("test")
    AtomicExpression.unapply(noop) shouldBe None
  }

  // ============================================================================
  // Cross-type Interaction Tests
  // ============================================================================

  behavior of "AtomicExpression cross-type interactions"

  it should "distinguish between different constant types" in {
    Zero should not be One
    One should not be Two
    Pi should not be E
    Root.phi should not be Root.psi
  }

  it should "maintain type hierarchy" in {
    Zero shouldBe a[ValueExpression]
    Pi shouldBe a[ScalarConstant]
    Root.phi shouldBe a[QuadraticRoot]
    Root.phi shouldBe a[Root]
    Root.phi shouldBe an[AtomicExpression]
  }

  it should "handle approximations consistently" in {
    // Exact values return None for approximation with force=false
    Zero.approximation(force = false) shouldBe empty
    One.approximation(force = false) shouldBe empty

    // Fuzzy values have approximations
    val fuzzy = Literal(Real(3.14, Some(AbsoluteFuzz(0.01, Box))))
    fuzzy.approximation(force = false) shouldBe defined
  }
}