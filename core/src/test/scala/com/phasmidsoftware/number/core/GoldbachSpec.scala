/*
 * Copyright (c) 2024. Phasmid Software
 */

package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GoldbachSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Goldbach"
  it should "goldbach success" in {
    Goldbach.goldbach(BigInt(16)) should matchPattern { case scala.util.Success(Prime(b1) -> Prime(b2)) if b1 == 3 && b2 == 13 => }
  }
  it should "goldbach fail" in {
    Goldbach.goldbach(BigInt(15)) should matchPattern { case scala.util.Failure(_) => }
  }
}
