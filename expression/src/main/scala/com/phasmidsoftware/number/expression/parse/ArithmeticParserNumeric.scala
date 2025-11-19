/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.inner.Rational
import scala.util.Try

/**
  * @author scalaprof
  */
abstract class ArithmeticParserNumeric[T: Numeric] extends ArithmeticParser[T] {
  self =>

  val num: Numeric[T] = implicitly[Numeric[T]]

  def div: (T, T) => T = num match {
    case value: Fractional[T] => value.div
    case x => throw new IllegalArgumentException(s"div method unavailable for ${x.getClass}")
  }

  def one: T = num.one

  def zero: T = num.zero

  def negate: T => T = num.negate

  def plus: (T, T) => T = num.plus

  def times: (T, T) => T = num.times
}

object DoubleArithmeticParser extends ArithmeticParserNumeric[Double] {
  def apply(s: String): Try[Double] = Try(s.toDouble)
}

object IntArithmeticParser extends ArithmeticParserNumeric[Int] {
  def apply(s: String): Try[Int] = Try(s.toInt)
}

object RationalArithmeticParser extends ArithmeticParserNumeric[Rational] {
  def apply(s: String): Try[Rational] = Rational.parse(s)
}

//object FuzzyExpressionParser extends ArithmeticParserNumeric[Fuzzy] {
//  def apply(s: String): Try[Fuzzy] = Try(Fuzzy.apply(s))
//}