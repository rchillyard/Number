/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class OperationsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MonadicOperationModulate (numbers)"
  it should "work" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, false)
    val (fInt, fRational, fDouble) = z.functions
    fInt(3) shouldBe Success(-1)
    fRational(Rational(3)) shouldBe Success(Rational.negOne)
    fDouble(3) shouldBe Success(-1.0)
  }
  it should "work with inclusive true" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, false)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }
  it should "work with inclusive false" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = false, false)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }

  behavior of "MonadicOperationModulate (angles)"
  it should "work" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, true)
    val (fInt, fRational, fDouble) = z.functions
    fInt(3) shouldBe Success(1)
    fRational(Rational(3)) shouldBe Success(Rational.one)
    fDouble(3) shouldBe Success(1.0)
  }
  it should "work with inclusive true" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, true)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }
  it should "work with inclusive false" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = false, true)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(1)
  }
}
