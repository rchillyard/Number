/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.algebraic.Algebraic.{phi, psi}
import com.phasmidsoftware.number.core.algebraic.Quadratic.phiApprox
import com.phasmidsoftware.number.core.inner.{Rational, SquareRoot, Value}
import com.phasmidsoftware.number.core.numerical.Number.{negOne, negate, one, two}
import com.phasmidsoftware.number.core.numerical.{Constants, Field, FuzzyEquality, Real}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuadraticSpec extends AnyFlatSpec with Matchers with FuzzyEquality {

  behavior of "Quadratic"
  private val phiReal: Real = Real(Constants.sPhi)
  it should "normalize" in {
    val expected: Real = phiReal
    val phiNormalized = phi.normalize
    phiNormalized.isSame(expected) shouldBe true
    val psiNormalized: Field = psi.normalize
    psiNormalized.isSame(Real(-0.618034)) shouldBe true
  }
  it should "branches" in {
    phi.equation.branches shouldBe 2
  }
  it should "isExact" in {
    phi.isExact shouldBe true
    psi.isExact shouldBe true
  }
  it should "value 1" in {
    val p: Algebraic = phi
    val solution: Solution = p.solve
    solution.base shouldBe Value.fromRational(Rational.half)
    solution.branch shouldBe 0
    solution.factor shouldBe SquareRoot
    import Rational.*
    solution.offset shouldBe Value.fromRational(r"5/4")
  }
  it should "value 2" in {
    val otherEquation = Quadratic(1, -1)
    val target = Algebraic_Quadratic(otherEquation, pos = true)
    val expected: Solution = phi.equation.invert.solve(0)
    val solution: Solution = target.solve
    solution shouldBe expected
    target.normalize.isSame(Real(0.618034)) shouldBe true
  }
  it should "value root 2" in {
    val p: Algebraic = Algebraic_Quadratic(Quadratic.rootTwoEquation, pos = true)
    p.render shouldBe "√2"
    val solution: Solution = p.solve
    solution.base shouldBe Value.zero
    solution.branch shouldBe 0
    solution.factor shouldBe SquareRoot
    solution.offset shouldBe Value.fromRational(Rational.two)
  }
  it should "render" in {
    phi.render shouldBe "\uD835\uDED7"
    psi.render shouldBe "\uD835\uDED9"
  }
  it should "approximation" in {
    val approximation = phi.approximation
    approximation.isDefined shouldBe true
    approximation.get.isSame(Real(phiApprox)) shouldBe true
  }
  it should "signum" in {
    phi.signum shouldBe 1
    psi.signum shouldBe -1
  }
  it should "abs" in {
    phi.abs.asReal shouldBe phi.asReal
    val actual = psi.abs.asNumber.map(x => x.toBigDecimal)
    val expected = psi.asNumber.map(x => x.doMultiple(-1).toBigDecimal)
    actual shouldBe expected
  }
  it should "scale" in {
    val actual = phi.scale(2)
    val expected = Algebraic_Quadratic(Quadratic(-2, -4), pos = true)
    actual shouldBe expected
  }
  it should "negate" in {
    phi.negate shouldBe Algebraic_Quadratic(Quadratic(1, -1), pos = false)
    val ok = for {
      a <- phi.negate.asReal
      b <- Constants.phi.asNumber.map(negate)
    } yield a === b
    ok shouldBe Some(true)
    psi.negate shouldBe Algebraic_Quadratic(Quadratic(1, -1), pos = true)
  }
  it should "product" in {
    val actual = phi * psi
    val expected: Rational = phi.equation.asInstanceOf[Quadratic].q
    actual shouldBe Algebraic_Linear(LinearEquation(expected))
    actual.render shouldBe "1"
  }
  it should "solutionSquared" in {
    val actual: Solution = phi.square.solve
    // XXX phi∧2 = phi + 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected: Solution = phi.solve.add(Rational.one) // NOTE this only works here
    actual shouldBe expected
  }
  it should "invert" in {
    val actual = phi.invert
    // XXX 1/phi = phi - 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected = phi `add` Real(-1)
    actual.isSame(expected) shouldBe true
  }
  it should "scale phi and psi with negative number" in {
    phi.scale(-1) shouldBe Algebraic_Quadratic(Quadratic(1, -1), pos = false)
  }
  // Test case for (fixed) Issue #127
  it should "add phi" in {
    val actual = phi.add(phi)
    val expected = phi.scale(2)
    actual shouldBe expected
  }
  it should "add phi and psi" in {
    val actual: Algebraic = phi `add` psi
    actual.solve.asField match {
      case Real(x) => x shouldBe one
    }
  }
  it should "add 1" in {
    val actual = phi `add` Real(one)
    actual.normalize should ===(phiReal + Real(1))
    actual.normalize.isSame(phiReal + Real(1)) shouldBe true
  }
  it should "add -1" in {
    println(s"phi = $phi")
    val actual = phi `add` Real(negOne)
    println(s"phi add Real(one) = $actual")
    actual.normalize should ===(phiReal + Real(-1))
    actual.normalize.isSame(phiReal + Real(-1)) shouldBe true
  }
  it should "add 2" in {
    println(s"phi = $phi")
    val actual = phi `add` Real(two)
    println(s"phi add Real(one) = $actual")
    actual.normalize should ===(phiReal + Real(2))
    actual.normalize.isSame(phiReal + Real(2)) shouldBe true
  }
  it should "multiply" in {
    val twoPhi = phi `multiply` Rational.two
    twoPhi.asReal.get should ===(Real(3.236067977499790))
  }
  it should "zero" in {
    val actual = Algebraic.zero.solve
    actual should matchPattern { case QuadraticSolution(Value.zero, Value.zero, SquareRoot, 0) => }
  }
  it should "one" in {
    val actual = Algebraic.one.solve
    actual should matchPattern { case QuadraticSolution(Value.one, Value.zero, SquareRoot, 0) => }
  }
  it should "power 0" in {
    val actual = phi.power(0)
    val expected = Real(1)
    actual shouldBe expected
    actual.normalize shouldBe Constants.one
  }
  it should "power 1" in {
    val actual = phi.power(1)
    val expected = phi
    println(s"phi=$phi; phi.power(1) = $actual")
    actual shouldBe expected
    actual.normalize should ===(phiReal)
  }
  it should "power 2" in {
    val actual = phi.power(2)
    val expected: Algebraic = phi.square
    actual shouldBe expected // TODO recreate test such that this works.
    actual.normalize should ===(phiReal + Real(1))
    actual.normalize.isSame(phiReal + Real(1)) shouldBe true
  }
  it should "power 3" in {
    val actual = phi.power(3)
    val scaled: Algebraic = phi.scale(2)
    val expected = scaled.equation.shiftOrigin(Rational.one).solve(0)
    println(s"phi∧3 = $actual. expected=$expected")
    val expectedValue = phiReal * Rational.two + Real(1)
    println(s"expectedValue = $expectedValue")
    actual.normalize should ===(expectedValue)
    actual.normalize.isSame(expectedValue) shouldBe true
  }
  it should "power 4" in {
    val actual = phi.power(4)
    val expected = phi.square.square
    println(s"phi∧3 = $actual. expected=$expected")
    actual.normalize should ===(expected)
    actual.normalize.isSame(expected) shouldBe true
  }
  it should "transform" in {
    val pFunc: (Rational, Rational) => Rational = (p, q) => (2 * q) - (p ∧ 2)
    val qFunc: (Rational, Rational) => Rational = (_, q) => q ∧ 2
    val equation = Quadratic.goldenRatioEquation
    println(s"equation = $equation")
    val quadratic = equation.transform(pFunc, qFunc)
    println(s"quadratic (transformed equation) = $quadratic")
    quadratic shouldBe Quadratic(-Rational.three, Rational.one)
    println(s"equation = $equation")
    println(quadratic.solve(0))
    println(quadratic.solve(1))
    println(s"quadratic.value = ${quadratic.solve(0)}")
  }
  it should "apply(Solution) to get phi" in {
    val phi = Algebraic.phi
    val solution: QuadraticSolution = phi.solve.asInstanceOf[QuadraticSolution]
    val algebraic: Algebraic_Quadratic = Algebraic_Quadratic.apply(solution)
    algebraic shouldBe phi
  }
  it should "apply(ExactNumber,ExactNumber) to get phi" in {
    val maybeAlgebraic = for {
      base <- Constants.half.asNumber
      offset <- (Constants.root5 `divide` Real(2)).asNumber
    } yield Algebraic_Quadratic.apply(base, offset, negative = false)
    maybeAlgebraic.isDefined shouldBe true
    maybeAlgebraic.get shouldBe phi
  }
}
