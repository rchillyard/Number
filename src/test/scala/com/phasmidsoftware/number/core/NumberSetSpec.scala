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

  it should "isMember (Number)" in {
    N.isMember(Number.one) shouldBe true
    Z.isMember(Number.zero) shouldBe true
    Q.isMember(Number.half) shouldBe true
    R.isMember(Number.pi) shouldBe true
    C.isMember(Number.i) shouldBe true
  }

  it should "isMember (Real)" in {
    N.isMember(Constants.one) shouldBe true
    Z.isMember(Constants.zero) shouldBe true
    Q.isMember(Constants.half) shouldBe true
    R.isMember(Constants.pi) shouldBe true
    C.isMember(Constants.i) shouldBe true
  }

  it should "belongsToSetExclusively" in {
    N.belongsToSetExclusively(Number.one) shouldBe true
    Z.belongsToSetExclusively(Number.zero) shouldBe true
    Q.belongsToSetExclusively(Number.half) shouldBe true
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
    C.belongsToSetExclusively(Number.half) shouldBe false
    R.belongsToSetExclusively(Number.one) shouldBe false
    C.belongsToSetExclusively(Number.zero) shouldBe false
    C.belongsToSetExclusively(Number.one) shouldBe false
  }

  behavior of "NumberLike"
  it should "memberOf" in {
    Number.one.memberOf shouldBe Some(N)
    Number.zero.memberOf shouldBe Some(Z)
    Number.half.memberOf shouldBe Some(Q)
    Number.pi.memberOf shouldBe Some(R)
    Number.i.memberOf shouldBe Some(C)
  }

  it should "memberOf(set)" in {
    Number.one.memberOf(N) shouldBe true
    Number.zero.memberOf(Z) shouldBe true
    Number.half.memberOf(Q) shouldBe true
    Number.pi.memberOf(R) shouldBe true
    Number.i.memberOf(C) shouldBe true
  }
}
