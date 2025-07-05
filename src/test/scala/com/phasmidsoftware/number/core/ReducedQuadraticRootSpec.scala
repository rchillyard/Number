/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.{root5, sPhi}
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.inner.{AnyContext, PureNumber, Rational}
import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.expression._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ReducedQuadraticRootSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "ReducedQuadraticRoot"

  val phi: ReducedQuadraticRoot = Phi
  it should "work from first principles" in {
    val target = new ReducedQuadraticRoot("\uD835\uDED7", -1, -1, true)
    target shouldBe Phi
    target.evaluateAsIs.isDefined shouldBe true
    target.evaluateAsIs.get should ===(Real(sPhi))
    target should matchPattern { case ReducedQuadraticRoot("\uD835\uDED7", -1, -1, true) => }
  }
  it should "evaluate Phi correctly" in {
    (((Phi ^ 2) - 1).materialize - Real(Constants.sPhi)).isZero shouldBe true
    Phi.materialize should ===(Constants.phi)
  }
  it should "evaluate Phi^2 correctly" in {
    val expression = Phi ^ 2
    val simplified = expression.simplify
    val z: Algebraic = Phi.asAlgebraic.add(Rational.one)
//    simplified shouldBe Literal(z)
    simplified.materialize should ===(2.618033988749895)
  }
  it should "evaluate Phi + Psi" in {
    val expression: Expression = Phi + Psi
    val simplified = expression.simplify
    simplified shouldBe One
  }
  it should "evaluate Phi * Psi" in {
    val expression = Phi * Psi
    val simplified = expression.simplify
    simplified shouldBe MinusOne
  }
  it should "evaluate Phi + Phi" in {
    val expression = Phi + Phi
    val simplified = expression.simplify
    val minus2 = Rational(-2)
    val minus4 = Rational(-4)
    simplified should matchPattern { case Literal(Algebraic_Quadratic(_, Quadratic(`minus2`, `minus4`), _), _) => }
    simplified.materialize should ===(3.23606797749979)
  }
  it should "evaluate 2 * Phi" in {
    val expression = Two * Phi
    val simplified = expression.simplify
    val minus2 = Rational(-2)
    val minus4 = Rational(-4)
    simplified should matchPattern { case Literal(Algebraic_Quadratic(_, Quadratic(`minus2`, `minus4`), _), _) => }
    simplified.materialize should ===(3.23606797749979)
  }
  it should "evaluate 2 + Phi" in {
    val expression = Two + Phi
    val simplified = expression.simplify
    val minus5 = Rational(-5)
    val plus5 = Rational(5)
    simplified should matchPattern { case Literal(Algebraic_Quadratic(_, Quadratic(minus5, plus5), _), _) => }
    simplified.materialize should ===(3.618033988749895)
  }

  it should "mind its ps and qs" in {
    phi.p shouldBe -1
    phi.q shouldBe -1
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

  it should "pos" in {
    phi.pos shouldBe true
  }

  it should "root" in {
    import com.phasmidsoftware.number.expression.Expression.ExpressionOps
    phi.root shouldBe ((One + root5) / 2)
    val maybeField: Option[Field] = phi.root.evaluateAsIs
    maybeField.isDefined shouldBe false
  }

  it should "maybeFactor" in {
    phi.maybeFactor shouldBe None
    ReducedQuadraticRoot.create(Algebraic.zero) flatMap (_.maybeFactor) shouldBe Some(PureNumber)
  }

  it should "isExact" in {
    phi.isExact shouldBe true
  }

  ignore should "render" in {
    phi.render shouldBe Real(sPhi)
  }

  it should "asNumber" in {
    val maybeNumber = phi.asNumber
    maybeNumber.isDefined shouldBe true
    maybeNumber.get.isSame(Number(sPhi)) shouldBe true
  }

  it should "create" in {
    val phi = Algebraic.phi
    ReducedQuadraticRoot.create(phi) shouldBe Some(Phi)
  }

  it should "depth" in {
    phi.depth shouldBe 1
  }

  it should "discriminant" in {
    phi.discriminant shouldBe 5
  }

  it should "isAtomic" in {
    phi.isAtomic shouldBe true
  }

  it should "evaluate(AnyContext)" in {
    phi.evaluate(AnyContext).get should ===(Real(sPhi))
  }

}
