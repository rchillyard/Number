package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.model
import com.phasmidsoftware.number.model.{Factor, Number, Scalar}

import scala.util.Try

/**
  * Parser for Number.
  */
class NumberParser extends RationalParser {

  /**
    * Parse the string w as a Number.
    * The string consists of two optional parts:
    * the numerator and the factor.
    * Either of these can be missing but not both.
    *
    * @param w the String to parse.
    * @return a Number, wrapped in Try.
    */
  def parseNumber(w: String): Try[model.Number] = parse(number, w)

  private def number: Parser[model.Number] = opt(rationalNumber) ~ opt(factor) ^^ { case ro ~ fo => optionalNumber(ro, fo).getOrElse(Number()) }

  private def optionalNumber(ro: Option[RationalNumber], fo: Option[Factor]) =
    if (ro.isDefined || fo.isDefined) for (r <- ro.orElse(Some(WholeNumber.one)); v <- r.value.toOption; f <- fo.orElse(Some(Scalar))) yield Number(v, f)
    else None

  import com.phasmidsoftware.number.model.Factor._

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) ^^ { w => Factor(w) }
}
