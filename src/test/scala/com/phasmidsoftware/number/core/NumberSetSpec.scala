package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NumberSetSpec extends AnyFlatSpec with should.Matchers {

  behavior of "NumberSet"

  it should "maybeSuperSet" in {
    C.maybeSuperSet shouldBe None
    R.maybeSuperSet shouldBe Some(C)
    Q.maybeSuperSet shouldBe Some(R)
    Z.maybeSuperSet shouldBe Some(Q)
    N.maybeSuperSet shouldBe Some(Z)
  }

  it should "isMember" in {
    N.isMember(Number.one) shouldBe true
    Z.isMember(Number.zero) shouldBe true
    Q.isMember(Number(Rational.half)) shouldBe true
    R.isMember(Number.pi) shouldBe true
    C.isMember(Number.i) shouldBe true
  }

  it should "belongsToSetExclusively" in {
    N.belongsToSetExclusively(Number.one) shouldBe true
    Z.belongsToSetExclusively(Number.zero) shouldBe true
    Q.belongsToSetExclusively(Number(Rational.half)) shouldBe true
    R.belongsToSetExclusively(Number.pi) shouldBe true
    C.belongsToSetExclusively(Number.i) shouldBe true
  }

  it should "not belongsToSetExclusively" in {
    Z.belongsToSetExclusively(Number.one) shouldBe false
    Q.belongsToSetExclusively(Number.zero) shouldBe false
    R.belongsToSetExclusively(Number(Rational.half)) shouldBe false
    C.belongsToSetExclusively(Number.pi) shouldBe false
    Q.belongsToSetExclusively(Number.one) shouldBe false
    R.belongsToSetExclusively(Number.zero) shouldBe false
    C.belongsToSetExclusively(Number(Rational.half)) shouldBe false
    R.belongsToSetExclusively(Number.one) shouldBe false
    C.belongsToSetExclusively(Number.zero) shouldBe false
    C.belongsToSetExclusively(Number.one) shouldBe false
  }

}
