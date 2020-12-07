package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.model
import com.phasmidsoftware.number.model.{Factor, Number, Scalar}

import scala.util.Try

class NumberParser extends RationalParser {

  def parseNumber(w: String): Try[model.Number] = parse(number, w)

  def number: Parser[model.Number] = rationalNumber ~ opt(factor) ^^ {
    case r ~ fo => (for (r <- r.value.toOption; f <- fo.orElse(Some(Scalar))) yield Number(r, f)).getOrElse(Number())
  }

  import com.phasmidsoftware.number.model.Factor._

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) ^^ { w => Factor(w) }

}
