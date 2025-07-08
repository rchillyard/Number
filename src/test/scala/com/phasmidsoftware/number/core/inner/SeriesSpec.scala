/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class SeriesSpec extends AnyFlatSpec {

  behavior of "FiniteSeries"

  val s = FiniteSeries(List(1, 2, 3, 4, 5, 6, 7))

  it should "term" in {
    s.term(0) shouldBe Some(1)
    s.term(1) shouldBe Some(2)
    s.term(2) shouldBe Some(3)
    s.term(6) shouldBe Some(7)
    s.term(7) shouldBe None
  }

  it should "evaluate n" in {
    s.evaluate(None) shouldBe Some(28)
  }

  it should "evaluate epsilon" in {

  }

  behavior of "InfiniteSeries"

  val t = InfiniteSeries(LazyList.from(1))

  it should "term" in {
    t.term(0) shouldBe Some(1)
    t.term(1) shouldBe Some(2)
    t.term(2) shouldBe Some(3)
    t.term(6) shouldBe Some(7)
    t.term(7) shouldBe Some(8)
  }

  it should "evaluate n" in {

  }

  it should "evaluate epsilon" in {

  }

}
