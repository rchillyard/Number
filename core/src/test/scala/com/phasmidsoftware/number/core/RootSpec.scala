/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.sPhi
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.core.expression.Root.{phi, psi}
import com.phasmidsoftware.number.core.expression._
import com.phasmidsoftware.number.core.inner.{AnyContext, PureNumber, Rational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RootSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "Root"
  it should "work from first principles" in {
    val target = Root(Quadratic(-1, -1), 0)
    target shouldBe phi
    target.evaluateAsIs.isDefined shouldBe true
    target.evaluateAsIs.get should ===(Real(sPhi))
  }
  it should "evaluate Phi correctly" in {
    (((phi ∧ 2) - 1).materialize - Real(Constants.sPhi)).isZero shouldBe true
    phi.materialize should ===(Constants.phi)
  }
  it should "evaluate Phi∧2 correctly" in {
    val expression = phi ∧ 2
    val simplified = expression.simplify
    val z: Algebraic = Algebraic.phi.add(Rational.one)
//    simplified shouldBe Literal(z)
    simplified.materialize should ===(2.618033988749895)
  }
  it should "evaluate Phi + Psi" in {
    val expression: Expression = phi + psi
    val simplified = expression.simplify
    simplified shouldBe One
  }
  it should "evaluate Phi * Psi" in {
    val expression = phi * psi
    val simplified = expression.simplify
    simplified shouldBe MinusOne
  }
  it should "evaluate Phi + Phi" in {
    val expression = phi + phi
    val simplified = expression.simplify
    val minus2 = Rational(-2)
    val minus4 = Rational(-4)
    simplified should matchPattern { case BiFunction(`phi`, Two, Product) => }
    simplified.materialize should ===(3.23606797749979)
  }
  it should "evaluate 2 * Phi" in {
    val expression = Two * phi
    val simplified = expression.simplify
    val minus2 = Rational(-2)
    val minus4 = Rational(-4)
    simplified should matchPattern { case Literal(Algebraic_Quadratic(_, Quadratic(`minus2`, `minus4`), _), _) => }
    simplified.materialize should ===(3.23606797749979)
  }
  it should "evaluate 2 + Phi" in {
    val expression = Two + phi
    val simplified = expression.simplify
    val minus5 = Rational(-5)
    val plus5 = Rational(5)
    simplified should matchPattern { case Literal(Algebraic_Quadratic(_, Quadratic(minus5, plus5), _), _) => }
    simplified.materialize should ===(3.618033988749895)
  }
  it should "mind its ps and qs" in {
    phi.equation.p shouldBe Rational.negOne
    phi.equation.q shouldBe Rational.negOne
  }
  it should "materialize" in {
    val materialized = phi.materialize
//    materialized.render shouldBe "\uD835\uDED7"
    materialized should ===(Real(sPhi))
    materialized.render.substring(0, 16) shouldBe sPhi.substring(0, 16)
  }
  it should "evaluateAsIs" in {
    // CONSIDER redefining evaluateAsIs so that it does not approximate
    phi.evaluateAsIs.get should ===(Real(sPhi))
//    phi.evaluateAsIs shouldBe None
  }
  it should "maybeFactor" in {
    phi.maybeFactor shouldBe None
    Root.zero.maybeFactor shouldBe Some(PureNumber)
  }
  it should "isExact" in {
    phi.isExact shouldBe true
  }
  it should "render" in {
    phi.render shouldBe "\uD835\uDED7"
  }
  it should "asNumber" in {
    val maybeNumber = phi.asNumber
    maybeNumber.isDefined shouldBe true
    maybeNumber.get.isSame(Number(sPhi)) shouldBe true
  }
  it should "depth" in {
    phi.depth shouldBe 1
  }
  it should "discriminant" in {
    phi.equation.discriminant shouldBe Rational(5)
  }
  it should "isAtomic" in {
    phi.isAtomic shouldBe true
  }
  it should "evaluate(AnyContext)" in {
    phi.evaluate(AnyContext).get should ===(Real(sPhi))
  }
}
