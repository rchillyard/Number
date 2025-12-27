/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.kernel.Eq
import org.scalatest.matchers.should.Matchers

trait AssertionHelpers extends Matchers {

  def assertEq[T: Eq](x: T, y: T): Unit = {
    Eq[T].eqv(x, y) shouldBe true : Unit
  }

  def assertNotEq[T: Eq](x: T, y: T): Unit = {
    Eq[T].eqv(x, y) shouldBe false : Unit
  }

}
