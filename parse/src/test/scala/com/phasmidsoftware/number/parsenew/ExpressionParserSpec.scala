/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.algebra.WholeNumber
import com.phasmidsoftware.number.expression.expr.*
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExpressionParser"

  import com.phasmidsoftware.number.parsenew.ExpressionParser.*

  it should "apply number expressions" in {
    lazymath"1" shouldBe One
    lazymath"-1" shouldBe UniFunction(One, Negate)
    lazymath"-(42)" shouldBe UniFunction(Literal(WholeNumber(42), Some("42")), Negate)
    lazymath"1+2" shouldBe BiFunction(One, Two, Sum)
    lazymath"6*7" shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product)
    lazymath"42/7" shouldBe BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product)
    lazymath"6*(3+4)" shouldBe BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product)
    lazymath"\frac{42}{6}" shouldBe BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product)
    lazymath"3^2" shouldBe BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power)
    lazymath"-3^2" shouldBe BiFunction(UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Literal(WholeNumber(2), Some("2")), Power)
    lazymath"2+3*4" shouldBe BiFunction(Two, BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Product), Sum)
    lazymath"3^2+4" shouldBe BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), Literal(WholeNumber(4), Some("4")), Sum)
    lazymath"3^2+4*5" shouldBe BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), BiFunction(Literal(WholeNumber(4), Some("4")), Literal(WholeNumber(5), Some("5")), Product), Sum)
  }
  // NOTE don't worry about highlighting issues here. The code is good.
  it should "apply pi" in {
    lazymath"""\pi""" shouldBe ConstPi
    lazymath"""2\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"""2*\pi""" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"""\pi^2""" shouldBe BiFunction(ConstPi, Two, Power)
  }
  it should "apply e" in {
    lazymath"""\e""" shouldBe ConstE
    lazymath"""\mathrm{e}""" shouldBe ConstE
    lazymath"""\e^2""" shouldBe BiFunction(ConstE, Two, Power)
  }
  it should "apply functions" in {
    lazymath"""\sqrt{2}""" shouldBe BiFunction(Two, Half, Power)
    lazymath"√2" shouldBe BiFunction(Two, Half, Power)
    lazymath"""\sin(\pi)""" shouldBe UniFunction(ConstPi, Sine)
    lazymath"""\cos(\pi)""" shouldBe UniFunction(ConstPi, Cosine)
    lazymath"""\tan(\pi)""" shouldBe BiFunction(UniFunction(ConstPi, Sine), UniFunction(UniFunction(ConstPi, Cosine), Reciprocal), Product)
    lazymath"""\ln(\e)""" shouldBe UniFunction(ConstE, Ln)
    lazymath"""\sin(\pi * -1)""" shouldBe UniFunction(BiFunction(ConstPi, UniFunction(One, Negate), Product), Sine)
    lazymath"""\exp(2)""" shouldBe UniFunction(Two, Exp)
  }
  it should "apply symbols" in {
    lazymath"½" shouldBe Half
    lazymath"2𝛑" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"2*π" shouldBe BiFunction(Two, ConstPi, Product)
    lazymath"∞" shouldBe Infinity
  }

}
