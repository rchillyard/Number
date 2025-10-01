/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Constants
import com.phasmidsoftware.number.core.inner.{Context, PureNumber, RestrictedContext, SquareRoot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RootSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Root"

  val phi = Root(Quadratic.goldenRatioEquation, 0)
  val psi = Root(Quadratic.goldenRatioEquation, 1)
  val one = Root(Quadratic(-2, 1), 0)
  val zero = Root(Quadratic(0, 0), 0)
  val rootTwo = Root(Quadratic.rootTwoEquation, 0)

  it should "evaluateAsIs" in {
    one.evaluateAsIs shouldBe Some(Constants.one)
    zero.evaluateAsIs shouldBe Some(Constants.zero)
    phi.evaluateAsIs shouldBe Some(Algebraic.phi)
    psi.evaluateAsIs shouldBe Some(Algebraic.psi)
  }

  it should "solution" in {
    phi.solution shouldBe QuadraticSolution.phi
  }

  it should "isExact" in {
    phi.isExact shouldBe true
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
  }

  it should "approximation" in {
    phi.approximation.get === Quadratic.phiApprox
    psi.approximation.get === 0.618033988749895
    one.approximation.get shouldBe Constants.one
    zero.approximation.get shouldBe Constants.zero
  }

  it should "maybeValue" in {
    phi.maybeValue shouldBe Some(Algebraic.phi)
    one.maybeValue shouldBe Some(Constants.one)
    zero.maybeValue shouldBe Some(Constants.zero)
  }

  it should "simplify" in {

  }

  it should "isAtomic" in {
    phi.isAtomic shouldBe true
    one.isAtomic shouldBe true
  }

  it should "simplifyAtomic" in {

  }

  it should "evaluate(PureNumber)" in {
    one.evaluate(RestrictedContext(PureNumber)) shouldBe Some(Constants.one)
    zero.evaluate(RestrictedContext(PureNumber)) shouldBe Some(Constants.zero)
    phi.evaluate(RestrictedContext(PureNumber)) shouldBe None
    psi.evaluate(RestrictedContext(PureNumber)) shouldBe None
  }

  it should "evaluate(NthRoot)" in {
    one.evaluate(Context.AnyRoot) shouldBe None
    phi.evaluate(Context.AnyRoot) shouldBe None
    rootTwo.evaluate(Context.AnyRoot) shouldBe Some(Constants.root2)
  }

}
