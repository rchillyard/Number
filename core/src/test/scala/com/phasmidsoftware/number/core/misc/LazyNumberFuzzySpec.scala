/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.misc.*
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * @author scalaprof
  */
class LazyNumberFuzzySpec extends flatspec.AnyFlatSpec with should.Matchers {

  import Fuzzy.*

  private val fuzz1 = LazyFuzzy(1)
  private val fuzz2 = LazyFuzzy(1, Product(2))

  def squ(x: Fuzzy): Fuzzy = x match {
    case f: FuzzyBase => f * x
  }

  private val fuzzSquare = Named[Fuzzy]("square", squ)
  //noinspection ScalaUnusedSymbol
  private val fuzz4 = fuzz2 `map` fuzzSquare
//  private val fuzzy = Exact(1)
  //noinspection ScalaUnusedSymbol
//  private val p = fuzzy * fuzzy

  "fuzz1" should "be 1" in {
    fuzz1.get shouldBe Fuzzy.one
  }

  it should "be -1 after negate" in {
    (-fuzz1).get shouldBe (Fuzzy.one * -1)
  }

  it should "be 0 after minus(1)" in {
    fuzz1.-(fuzz1).get shouldBe Fuzzy.zero
  }

  "fuzz2" should "be 2" in {
    fuzz2.get shouldBe (Fuzzy.one + Fuzzy.one)
  }

  it should "be 4 when multiplied by itself" in {
    (fuzz2 * fuzz2).get shouldBe Exact(4)
  }

  it should "be 1 when divided by itself" in {
    (fuzz2 / fuzz2).get shouldBe Fuzzy.one
  }

  it should "be 3 when added to one" in {
    (fuzz2 + fuzz1).get shouldBe Exact(3)
  }

  it should "be 6 when added to one and three" in {
    (fuzz2 + fuzz1 + LazyFuzzy(Exact(3))).get shouldBe Exact(6)
  }

  it should "be 3 when added to one by explicit function" in {
    //val lr = fuzz2 map Named("add Rat.1",{ x => x + Fuzzy.one })
    val lr = fuzz2 `map` Named("add Rat.1", { (x: Fuzzy) => Fuzzy.sum(x, Fuzzy.one) })  //fixed
    lr.get shouldBe (Fuzzy.one * 3)
  }

  "fuzzy for comprehension" should "give 4" in {
    val z = for (x <- fuzz2) yield fuzzSquare(x)
    z.get should be(Exact(4))
  }

  behavior of "fuzzy composition" //fixed
  it should "work for e^1" in {
    val p = fuzz1.map(ExpDifferentiable[Fuzzy]()(Fuzzy.FuzzyNumeric))
    p.get shouldBe Exact(Math.E)
  }
  // exp(2) is not substituted, so it comes from math.exp, which is accurate to within one ulp
  // and is not required to be reproducible across platforms. Comparing within a tolerance makes
  // the test true on every platform, so it needs no tag: the tolerance below is far wider than
  // one ulp and far tighter than any error worth catching.
  it should "work for e^2" in {
    val p = LazyFuzzy(2).map(ExpDifferentiable[Fuzzy]()(Fuzzy.FuzzyNumeric))
    p.get match {
      case Exact(y) => y shouldBe 7.38905609893065 +- 1E-12
      case other => fail(s"expected an Exact value but got: $other")
    }
  }
  it should "work with fuzzy 1" in {
    val f = LazyFuzzy(Bounded(1, 1E-3))
    val p = f.map(ExpDifferentiable[Fuzzy]()(FuzzyNumeric))
    p.get shouldBe Bounded(Math.E, 1E-3)
  }

  it should "give 8" in {
    val z = for (x <- fuzz2; y <- fuzz4) yield FuzzyNumeric.times(x, y)
    z.get should be(Exact(8))
  }

}