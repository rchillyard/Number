package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.Slow
import org.scalatest.flatspec
import org.scalatest.matchers.should

/**
  * @author scalaprof
  */
class LazyNumberFuzzySpec extends flatspec.AnyFlatSpec with should.Matchers {

  import Fuzzy._

  private val fuzz1 = LazyFuzzy(1)
  private val fuzz2 = LazyFuzzy(1, Product(2))

  def squ(x: Fuzzy): Fuzzy = x match {
    case f: FuzzyBase => f * x
    case _ => throw new Exception("Logic error")
  }

  private val fuzzSquare = Named[Fuzzy]("square", squ)
  //noinspection ScalaUnusedSymbol
  private val fuzz4 = fuzz2 map fuzzSquare
  private val fuzzy = Exact(1)
  //noinspection ScalaUnusedSymbol
  private val p = fuzzy * fuzzy

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
    val lr = fuzz2 map Named("add Rat.1", { x: Fuzzy => Fuzzy.sum(x, Fuzzy.one) })  //fixed
    lr.get shouldBe (Fuzzy.one * 3)
  }

  "fuzzy for comprehension" should "give 4" in {
    val z = for (x <- fuzz2) yield fuzzSquare(x)
    z.get should be(Exact(4))
  }

  behavior of "fuzzy composition" //fixed
  // Fails on CircleCI
  it should "work" taggedAs(Slow) in {
    val p = fuzz1.map(ExpDifferentiable[Fuzzy]()(Fuzzy.FuzzyNumeric))
    val z = p.get
//    z should be(2.718281828459045)
    p.get shouldBe Exact(2.7182818284590455)
  }
  // Fails on CircleCI
  it should "work with fuzzy 1" taggedAs(Slow) in {
    val f = LazyFuzzy(Bounded(1, 1E-3))
    val p = f.map(ExpDifferentiable[Fuzzy]()(FuzzyNumeric))
//    p.get should be(2.718281828459045)
    p.get shouldBe Bounded(2.7182818284590455, 1E-3)
  }

  it should "give 8" in {
    val z = for (x <- fuzz2; y <- fuzz4 ) yield FuzzyNumeric.times(x, y)
    z.get should be (Exact(8))
  }

}