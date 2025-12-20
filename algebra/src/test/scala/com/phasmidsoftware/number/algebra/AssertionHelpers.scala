/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import cats.kernel.Eq
import org.scalatest.matchers.should.Matchers

trait AssertionHelpers extends Matchers {

  def assertEq[T: Eq](x: T, y: T): Unit = {
    Eq[T].eqv(x, y) shouldBe true
  }

  def assertNotEq[T: Eq](x: T, y: T): Unit = {
    Eq[T].eqv(x, y) shouldBe false
  }

}
