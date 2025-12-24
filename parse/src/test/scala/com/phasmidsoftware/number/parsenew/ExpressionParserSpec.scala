/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.algebra.eager.{Angle, NatLog, WholeNumber}
import com.phasmidsoftware.number.expression.expr.*
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExpressionParser"

  import com.phasmidsoftware.number.parsenew.ExpressionParser.*

  it should "puremath number expressions" in {
    puremath"-1" shouldBe UniFunction(One, Negate)
    puremath"-(42)" shouldBe UniFunction(Literal(WholeNumber(42), Some("42")), Negate)
    puremath"1+2" shouldBe BiFunction(One, Two, Sum)
    puremath"6*7" shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product)
    puremath"42/7" shouldBe BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product)
    puremath"6*(3+4)" shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product)
    puremath"\frac{42}{6}" shouldBe BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product)
    puremath"3^2" shouldBe BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power)
    puremath"-3^2" shouldBe BiFunction(UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Literal(WholeNumber(2), Some("2")), Power)
    puremath"2+3*4" shouldBe BiFunction(Two, BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Product), Sum)
    puremath"3^2+4" shouldBe BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), Literal(WholeNumber(4), Some("4")), Sum)
    puremath"3^2+4*5" shouldBe BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), BiFunction(Literal(WholeNumber(4), Some("4")), Literal(WholeNumber(5), Some("5")), Product), Sum)
  }
  it should "lazymath number expressions" in {
    lazymath"1" shouldBe One
    lazymath"-1" shouldBe MinusOne
    lazymath"-(42)" shouldBe Literal(WholeNumber(-42), Some("-42"))
    lazymath"1+2" shouldBe Literal(WholeNumber(3), Some("3"))
    lazymath"6*7" shouldBe Literal(WholeNumber(42), Some("42"))
    lazymath"42/7" shouldBe Literal(WholeNumber(6), Some("6"))
    lazymath"6*(3+4)" shouldBe Literal(WholeNumber(42), Some("42"))
    lazymath"\frac{42}{6}" shouldBe Literal(WholeNumber(7), Some("7"))
    lazymath"3^2" shouldBe Literal(WholeNumber(9), Some("9"))
    lazymath"-3^2" shouldBe Literal(WholeNumber(9), Some("9"))
    lazymath"2+3*4" shouldBe Literal(WholeNumber(14), Some("14"))
    lazymath"3^2+4" shouldBe Literal(WholeNumber(13), Some("13"))
    lazymath"3^2+4*5" shouldBe Literal(WholeNumber(29), Some("29"))
  }
  // NOTE don't worry about highlighting issues here. The code is good.
  it should "puremath pi" in {
    puremath"""\pi""" shouldBe ConstPi
    puremath"""2\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    puremath"""2*\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    puremath"""\pi^2""" shouldBe BiFunction(ConstPi, Two, Power)
  }
  it should "lazymath pi" in {
    lazymath"""\pi""" shouldBe ConstPi
    lazymath"""2\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"""2*\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"""\pi^2""" shouldBe BiFunction(ConstPi, Two, Power)
  }
  it should "puremath e" in {
    puremath"""\e""" shouldBe ConstE
    puremath"""\mathrm{e}""" shouldBe ConstE
    puremath"""\e^2""" shouldBe BiFunction(ConstE, Two, Power)
  }
  it should "lazymath e" in {
    lazymath"""\e""" shouldBe ConstE
    lazymath"""\mathrm{e}""" shouldBe ConstE
    lazymath"""\e^2""" shouldBe Literal(NatLog(WholeNumber(2)), Some("e^2"))
  }
  it should "puremath functions" in {
    puremath"""\sin(\pi)""" shouldBe UniFunction(ConstPi, Sine)
    puremath"""\cos(\pi)""" shouldBe UniFunction(ConstPi, Cosine)
    puremath"""\tan(\pi)""" shouldBe BiFunction(UniFunction(ConstPi, Sine), UniFunction(UniFunction(ConstPi, Cosine), Reciprocal), Product)
    puremath"""\ln(\e)""" shouldBe UniFunction(ConstE, Ln)
    puremath"""\sin(\pi * -1)""" shouldBe UniFunction(BiFunction(ConstPi, UniFunction(One, Negate), Product), Sine)
  }
  it should "lazymath functions" in {
    lazymath"""\sqrt{2}""" shouldBe BiFunction(Two, Half, Power)
    lazymath"√2" shouldBe BiFunction(Two, Half, Power)
    lazymath"""\sin(\pi)""" shouldBe Zero
    lazymath"""\cos(\pi)""" shouldBe MinusOne
    lazymath"""\tan(\pi)""" shouldBe Zero
    lazymath"""\ln(\e)""" shouldBe One
    lazymath"""\sin(\pi * -1)""" shouldBe Zero
    lazymath"""\exp(2)""" shouldBe UniFunction(Two, Exp)
  }
  it should "puremath symbols" in {
    puremath"½" shouldBe Half
    puremath"2𝛑" shouldBe BiFunction(Two, ConstPi, Product)
    puremath"2*π" shouldBe BiFunction(Two, ConstPi, Product)
    puremath"∞" shouldBe Infinity
  }
  it should "lazymath symbols" in {
    lazymath"½" shouldBe Half
    lazymath"2𝛑" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"2*π" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"∞" shouldBe Infinity
  }

}
