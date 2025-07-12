/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Series
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try

class OldPowerSeriesSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "PowerSeries"

  val p2: NumberPolynomial = NumberPolynomial(5, 3, 1)
  val p4: NumberPolynomial = NumberPolynomial(3, 2, 5, 4, 1)

  it should "asReal" in {

  }

  it should "isSame" in {

  }

  it should "signum" in {

  }

  it should "evaluateToTolerance i" in {

  }

  it should "evaluateToTolerance x" in {

  }

  it should "isZero" in {

  }

  it should "isExact" in {

  }

  it should "asNumber" in {

  }

  it should "getTerm" in {

  }

  it should "approximation 1" in {

  }

  it should "approximation 2" in {

  }

  it should "toString" in {
  }

  ignore should "render" in {
  }

  it should "isUnity" in {

  }

}
