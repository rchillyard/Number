/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.numerical.Complex
import com.phasmidsoftware.number.expression.expr.ExpressionException

import scala.util.Try

/**
  * A parser of Rational objects.
  */
class ComplexParser extends BaseNumberParser {

  def doParse(w: String): Try[Complex] = parseAll(complexNumber, w) match {
    case Success(c, _) => scala.util.Success(c)
    case Failure(m, x) => scala.util.Failure(ExpressionException(s"ComplexParser.parse: unable to parse '$w' because $m:$x"))
    case Error(m, x) => scala.util.Failure(ExpressionException(s"ComplexParser.parse: unable to parse '$w' because $m:$x"))
  }

  private def complexNumber: Parser[Complex] = (opt(number) ~ opt(sign) ~ opt("i" ~> number)) :| "complexNumber" ^^ {
    case maybeR ~ maybeSign ~ maybeI => Complex.create(maybeR, maybeSign, maybeI)
  }

  val sign: Parser[String] = "+" | "-"
}

object ComplexParser {


  def parse(w: String): Try[Complex] = new ComplexParser().doParse(w)
}