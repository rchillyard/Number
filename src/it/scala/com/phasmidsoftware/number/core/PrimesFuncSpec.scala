package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * CONSIDER moving this to an integration testing directory since it is a bit slow.
 */
class PrimesFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"
  it should "isCarmichaelNumber1" in {
    Prime.isCarmichaelNumber(1) shouldBe false
    Prime.isCarmichaelNumber(2) shouldBe false
    Prime.isCarmichaelNumber(3) shouldBe false
    Prime.isCarmichaelNumber(41) shouldBe false
    Prime.isCarmichaelNumber(561) shouldBe true
    Prime.isCarmichaelNumber(1729) shouldBe true
  }
}
