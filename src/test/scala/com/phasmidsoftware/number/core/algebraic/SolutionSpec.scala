/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.inner.{Rational, SquareRoot, Value}
import com.phasmidsoftware.number.expression.Root
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SolutionSpec extends AnyFlatSpec {

  behavior of "Solution"

  val phi: Solution = Root.phi.solution
  val psi: Solution = Root.psi.solution
  val root2: Solution = Root.rootTwo.solution
  val negRoot2: Solution = Root.negRootTwo.solution

  it should "isPureNumber" in {
    phi.isPureNumber shouldBe false
    psi.isPureNumber shouldBe false
    root2.isPureNumber shouldBe false
    negRoot2.isPureNumber shouldBe false
  }

  it should "base" in {
    phi.base shouldBe Value.half
    psi.base shouldBe Value.half
    root2.base shouldBe Value.zero
    negRoot2.base shouldBe Value.zero
  }

  it should "scale" in {
    phi.scale(2) flatMap (_.scale(Rational.half)) shouldBe Some(phi)
  }

  it should "branch" in {
    phi.branch shouldBe 0
    psi.branch shouldBe 1
    root2.branch shouldBe 0
    negRoot2.branch shouldBe 1
  }

  it should "factor" in {
    phi.factor shouldBe SquareRoot
    psi.factor shouldBe SquareRoot
    root2.factor shouldBe SquareRoot
    negRoot2.factor shouldBe SquareRoot
  }

  it should "isZero" in {
    phi.isZero shouldBe false
    psi.isZero shouldBe false
    root2.isZero shouldBe false
    negRoot2.isZero shouldBe false
  }

  it should "maybeFactor" in {
    phi.maybeFactor shouldBe None
    psi.maybeFactor shouldBe None
    root2.maybeFactor shouldBe Some(SquareRoot)
    negRoot2.maybeFactor shouldBe Some(SquareRoot)
  }

  it should "isExact" in {
    phi.isExact shouldBe true
    psi.isExact shouldBe true
    root2.isExact shouldBe true
    negRoot2.isExact shouldBe true
  }

  it should "signum" in {
    phi.signum shouldBe 1
    psi.signum shouldBe -1
    root2.signum shouldBe 1
    negRoot2.signum shouldBe -1
  }

  it should "add" in {
    phi.add(psi) shouldBe Some(LinearSolution(Value.one))
    root2.add(negRoot2) shouldBe Some(LinearSolution(Value.zero))
  }

  it should "offset" in {
    phi.offset shouldBe Value.fromRational(Rational(5, 4))
    psi.offset shouldBe Value.fromRational(Rational(5, 4))
    root2.offset shouldBe Value.two
    negRoot2.offset shouldBe Value.two
  }

  it should "isUnity" in {
    phi.isUnity shouldBe false
    psi.isUnity shouldBe false
    phi.isUnity shouldBe false
    psi.isUnity shouldBe false
  }

  it should "asNumber" in {

  }

  it should "asField" in {

  }

}
