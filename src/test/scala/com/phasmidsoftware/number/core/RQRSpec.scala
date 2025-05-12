/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{root5, two}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RQRSpec extends AnyFlatSpec with Matchers {

  behavior of "RQR"

  it should "normalize" in {
    val expected: Real = Constants.phi
    RQR.phi.normalize.isSame(expected) shouldBe true
  }

  it should "branches" in {
    RQR.phi.branches shouldBe 2
  }

  it should "isExact" in {
    RQR.phi.isExact shouldBe true
    RQR.psi.isExact shouldBe false
  }

  it should "value" in {
    val p = RQR.phi
    val (n1, n2) = p.value
    n1 shouldBe Number.half
    val resultingValue: Option[Number] = n2 map (_.doMultiply(two))
    val expected: Option[Number] = Some(root5)
    // FIXME In the following we use isSame even though they should actually equal.
    // The fix is commented out in GeneralNumber.times.
    // However, that causes a raft of failures elsewhere.
    // More investigation is needed.
    for (x <- resultingValue; y <- expected) x.isSame(y) shouldBe true
  }
}
