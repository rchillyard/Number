/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Constants
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.expression.Literal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RootSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Root"

  val phi: QuadraticRoot = Root.phi
  val psi: QuadraticRoot = Root.psi
  val one: QuadraticRoot = Root.one
  val zero: QuadraticRoot = Root.zero
  val rootTwo: QuadraticRoot = Root.rootTwo
  val half: LinearRoot = Root.half

  it should "evaluateAsIs" in {
    one.evaluateAsIs shouldBe Some(Constants.one)
    zero.evaluateAsIs shouldBe Some(Constants.zero)
    phi.evaluateAsIs shouldBe Some(Algebraic.phi)
    psi.evaluateAsIs shouldBe Some(Algebraic.psi)
    rootTwo.evaluateAsIs shouldBe Some(Constants.root2)
    half.evaluateAsIs shouldBe Some(Constants.half)
  }

  it should "solution" in {
    phi.solution shouldBe QuadraticSolution.phi
    rootTwo.solution shouldBe QuadraticSolution(Value.zero, Value.fromInt(2), SquareRoot, 0)
  }

  it should "isExact" in {
    phi.isExact shouldBe true
    rootTwo.isExact shouldBe true
  }

  it should "maybeFactor" in {
    phi.maybeFactor shouldBe None
    one.maybeFactor shouldBe Some(PureNumber)
    rootTwo.maybeFactor shouldBe Some(SquareRoot)
  }

  it should "render" in {
    phi.render shouldBe "\uD835\uDED7"
    psi.render shouldBe "𝛙"
    one.render shouldBe "1"
    zero.render shouldBe "0"
    rootTwo.render shouldBe "√2"
    half.render shouldBe "½"
  }

  it should "approximation" in {
    phi.approximation.get === Quadratic.phiApprox
    psi.approximation.get === 0.618033988749895
    one.approximation.get shouldBe Constants.one
    zero.approximation.get shouldBe Constants.zero
    rootTwo.approximation.get === 1.4142135623730951
    half.approximation.get === 0.5
  }

  it should "maybeValue" in {
    phi.maybeValue shouldBe Some(Algebraic.phi)
    one.maybeValue shouldBe Some(Constants.one)
    zero.maybeValue shouldBe Some(Constants.zero)
    rootTwo.maybeValue shouldBe Some(Constants.root2)
    half.maybeValue shouldBe Some(Constants.half)
  }

  it should "simplify" in {

  }

  it should "isAtomic" in {
    phi.isAtomic shouldBe true
    one.isAtomic shouldBe true
    half.isAtomic shouldBe true
  }

  it should "simplifyAtomic" in {

  }

  it should "evaluate(PureNumber)" in {
    one.evaluate(RestrictedContext(PureNumber)) shouldBe Some(Constants.one)
    zero.evaluate(RestrictedContext(PureNumber)) shouldBe Some(Constants.zero)
    phi.evaluate(RestrictedContext(PureNumber)) shouldBe None
    psi.evaluate(RestrictedContext(PureNumber)) shouldBe None
    rootTwo.evaluate(RestrictedContext(PureNumber)) shouldBe None
    half.evaluate(RestrictedContext(PureNumber)) shouldBe Some(Constants.half)
  }

  it should "evaluate(NthRoot)" in {
    one.evaluate(Context.AnyRoot) shouldBe None
    phi.evaluate(Context.AnyRoot) shouldBe None
    rootTwo.evaluate(Context.AnyRoot) shouldBe Some(Constants.root2)
    half.evaluate(Context.AnyRoot) shouldBe None
  }

  behavior of "Root expressions"

  it should "evaluate 1 / √5" in {
    import com.phasmidsoftware.number.expression.Expression.ExpressionOps
    val expression = (phi + psi) / (phi - psi)
    val simplified = expression.simplify
    simplified.materialize === Constants.root5.invert
  }

  it should "evaluate phi^2" in {
    import com.phasmidsoftware.number.expression.Expression.ExpressionOps
    val expression = phi ^ 2
    val simplified = expression.simplify
    simplified.materialize === 2.618033988749895
    simplified shouldBe Literal(Algebraic_Quadratic(Quadratic(-3, 1), pos = true))
  }

  it should "evaluate squareRoot(phi)" in {
    import com.phasmidsoftware.number.expression.Expression.ExpressionOps
    val expression = phi ^ Rational.half
    val simplified = expression.simplify
    simplified.approximation.get === 1.2599210498948732
    simplified shouldBe QuadraticRoot(Quadratic(1, -1), 0)
  }

  // FIXME Issue #130
  ignore should "evaluate 2" in {
    import com.phasmidsoftware.number.expression.Expression.ExpressionOps
    val expression = (phi + psi) / (phi - psi)
    val simplified = expression.simplify
    simplified shouldBe Literal(Constants.root5).reciprocal
  }
}
