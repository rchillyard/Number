/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.{root5, sPhi}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ReducedQuadraticRootSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ReducedQuadraticRoot"

  val phi: ReducedQuadraticRoot = Phi

  it should "mind its ps and qs" in {
    phi.p shouldBe -1
    phi.q shouldBe -1
  }

  ignore should "simplifyAndEvaluate" in {
    phi.materialize shouldBe Real(sPhi)
  }

  it should "substituteTerms" in {

  }

  it should "isExactInContext" in {

  }

  ignore should "evaluateAsIs" in {
    phi.evaluateAsIs shouldBe Real(sPhi)
  }

  it should "pos" in {

  }

  it should "root" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    phi.root shouldBe ((One + root5) / 2)
    val maybeField: Option[Field] = phi.root.evaluateAsIs
    maybeField.isDefined shouldBe true
    maybeField.get.isSame(Real(sPhi)) shouldBe true
  }

  it should "context" in {

  }

  it should "maybeFactor" in {

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

  it should "depth" in {

  }

  it should "discriminant" in {
    phi.discriminant shouldBe 5
  }

  it should "isAtomic" in {

  }

  it should "evaluate" in {

  }

}
