/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NumberLikeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "NumberLike"
  it should "asJavaNumber" in {
    Constants.zero.asJavaNumber shouldBe Some(Int.box(0))
    Constants.half.asJavaNumber shouldBe Some(Double.box(0.5))
    // CONSIDER is the following the desired behavior?
    Rational.third.asJavaNumber shouldBe Some(Double.box(1.0 / 3))
    Constants.pi.asJavaNumber shouldBe None
    Constants.e.asJavaNumber shouldBe None
  }
  it should "asNumber" in {

  }
  it should "maybeFactor" in {

  }
  it should "isExact" in {

  }
  it should "memberOf" in {

  }
  it should "render" in {
    Constants.zero.render shouldBe "0"
    Constants.half.render shouldBe "½"
    Constants.pi.render shouldBe "\uD835\uDED1"
    Constants.e.render shouldBe "\uD835\uDF00"
    Constants.root2.render shouldBe "√2"
  }
}
