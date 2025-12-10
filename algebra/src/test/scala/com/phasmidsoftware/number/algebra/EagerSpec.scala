/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EagerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Eager"

  it should "~=" in {
    val x: Eager = Real(1.0)
    val y: Eager = Real(1.0)
    x should ===(y)
  }

  // TODO add more tests, especially comparing Real(1) with WholeNumber(1),
}
