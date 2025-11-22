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
    math"1" shouldBe Some(One)
    math"-1" shouldBe Some(UniFunction(One, Negate))
    math"-(42)" shouldBe Some(UniFunction(Literal(WholeNumber(42), Some("42")), Negate))
    math"1+2" shouldBe Some(BiFunction(One, Two, Sum))
    math"6*7" shouldBe Some(BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product))
    math"42/7" shouldBe Some(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product))
    math"6*(3+4)" shouldBe Some(BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product))
    math"""\frac{42}{6}""" shouldBe Some(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product))
    math"3^2" shouldBe Some(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power))
    math"-3^2" shouldBe Some(BiFunction(UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Literal(WholeNumber(2), Some("2")), Power))
    math"2+3*4" shouldBe Some(BiFunction(Two, BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Product), Sum))
    math"3^2+4" shouldBe Some(BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), Literal(WholeNumber(4), Some("4")), Sum))
    math"3^2+4*5" shouldBe Some(BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), BiFunction(Literal(WholeNumber(4), Some("4")), Literal(WholeNumber(5), Some("5")), Product), Sum))
  }
  // NOTE don't worry about highlighting issues here. The code is good.
  it should "apply pi" in {
    math"""\pi""" shouldBe Some(ConstPi)
    math"""2\pi""" shouldBe Some(BiFunction(Two, ConstPi, Product))
    math"""2*\pi""" shouldBe Some(BiFunction(Two, ConstPi, Product))
    math"""\pi^2""" shouldBe Some(BiFunction(ConstPi, Two, Power))
  }
  it should "apply e" in {
    math"""\e""" shouldBe Some(ConstE)
    math"""\mathrm{e}""" shouldBe Some(ConstE)
    math"""\e^2""" shouldBe Some(BiFunction(ConstE, Two, Power))
  }
  it should "apply functions" in {
    math"""\sqrt{2}""" shouldBe Some(BiFunction(Two, Half, Power))
    math"""√2""" shouldBe Some(BiFunction(Two, Half, Power))
    math"""\sin(\pi)""" shouldBe Some(UniFunction(ConstPi, Sine))
    math"""\cos(\pi)""" shouldBe Some(UniFunction(ConstPi, Cosine))
    math"""\tan(\pi)""" shouldBe Some(BiFunction(UniFunction(ConstPi, Sine), UniFunction(UniFunction(ConstPi, Cosine), Reciprocal), Product))
    math"""\ln(\e)""" shouldBe Some(UniFunction(ConstE, Ln))
    math"""\sin(\pi * -1)""" shouldBe Some(UniFunction(BiFunction(ConstPi, UniFunction(One,Negate),Product),Sine ))
    math"""\exp(2)""" shouldBe Some(UniFunction(Two,Exp))
  }

}
