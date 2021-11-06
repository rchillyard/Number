package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.Rational

import scala.util.Try

trait ValuableNumber {

  /**
    * Evaluate this RationalNumber as a Try[Rational].
    *
    * @return a Try[Rational]
    */
  def value: Try[Rational]
}

/**
  * A parser of Rational objects.
  */
abstract class BaseRationalParser extends SignificantSpaceParsers {

  case class WholeNumber(sign: Boolean, digits: String) extends ValuableNumber {
    def value: Try[Rational] = scala.util.Success(Rational(BigInt(digits)).applySign(sign))
  }

  object WholeNumber {
    val one: WholeNumber = WholeNumber(sign = false, "1")
  }

  case class RatioNumber(numerator: WholeNumber, denominator: WholeNumber) extends ValuableNumber {
    def value: Try[Rational] = for (n <- numerator.value; d <- denominator.value) yield n / d
  }

  case class RealNumber(sign: Boolean, integerPart: String, maybeFractionalPart: Option[String], exponent: Option[String]) extends ValuableNumber {
    val fractionalPart: String = maybeFractionalPart.getOrElse("")

    def value: Try[Rational] = {
      val bigInt = BigInt(integerPart + fractionalPart)
      val exp = exponent.getOrElse("0").toInt
      Try(Rational(bigInt).applySign(sign).applyExponent(exp - fractionalPart.length))
    }
  }

  def rationalNumber: Parser[ValuableNumber] = (realNumber | ratioNumber) :| "rationalNumber"

  def ratioNumber: Parser[RatioNumber] = (simpleNumber ~ opt("/" ~> simpleNumber)) :| "ratioNumber" ^^ {
    case n ~ maybeD => RatioNumber(n, maybeD.getOrElse(WholeNumber.one))
  }

  def simpleNumber: Parser[WholeNumber] = (opt("-") ~ unsignedWholeNumber) :| "simpleNumber" ^^ {
    case so ~ n => WholeNumber(so.isDefined, n)
  }

  def realNumber: Parser[RealNumber] = (opt("-") ~ unsignedWholeNumber ~ ("." ~> opt(unsignedWholeNumber)) ~ opt(E ~> wholeNumber)) :| "realNumber" ^^ {
    case so ~ integerPart ~ fractionalPart ~ expo => RealNumber(so.isDefined, integerPart, fractionalPart, expo)
  }

  /** An integer, without sign. */
  def unsignedWholeNumber: Parser[String] = logit("""\d+""".r)("unsignedWholeNumber") //^^ (x => debug(s"unsignedWholeNumber",x))

  private val E = "[eE]".r

}

object RationalParser extends BaseRationalParser {
  def parse(s: String): Try[Rational] = stringParser(rationalNumber, s).flatMap(_.value)
}

case class RationalParserException(m: String) extends Exception(m)
