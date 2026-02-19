/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.inner.{Rational, VulgarFraction}

import scala.util.Try

/**
  * Represents a numerical value that can be evaluated as a `Rational` number
  * wrapped in a `Try`, indicating possible failure during parsing.
  */
trait ValuableNumber {

  /**
    * Evaluate this RationalNumber as a Try[Rational].
    *
    * @return a Try[Rational]
    */
  def value: Try[Rational]
}

/**
  * Represents a repeating decimal number, including its sign, integer part,
  * non-repeating fractional part (if any), and repeating fractional part.
  *
  * This class models numbers that can be expressed as decimals with repeating
  * cycles, providing functionality to compute their exact rational representation.
  *
  * @constructor Creates a new instance of the RepeatingDecimal.
  * @param sign             Specifies the sign of the number (true for positive, false for negative).
  * @param integerPart      The integer portion of the decimal number.
  * @param nonRepeatingPart The non-repeating fractional part of the decimal (optional).
  * @param repeatingPart    The repeating fractional part of the decimal.
  */
case class RepeatingDecimal(sign: Boolean, integerPart: String, nonRepeatingPart: Option[String], repeatingPart: String) extends ValuableNumber {
  /**
    * Computes the rational value represented by the repeating decimal, including:
    * - the integer part,
    * - the non-repeating fractional part (if present),
    * - the repeating fractional part.
    *
    * The result is a signed `Rational` that combines all parts based on the properties
    * of the repeating decimal.
    *
    * @return `Try` containing the computed `Rational` representation of the repeating decimal,
    *         or a failure if any error occurs during computation.
    */
  def value: Try[Rational] = Try {
    val intPortion = Rational(BigInt(integerPart))

    val nonRepLength = nonRepeatingPart.map(_.length).getOrElse(0)
    val nonRepPortion = nonRepeatingPart.map { nr =>
      Rational(BigInt(nr)).applyExponent(-nr.length)
    }.getOrElse(Rational.zero)

    // Repeating part: e.g., "3" repeating = 3/9
    // Position: starts after non-repeating part
    val repNumerator = BigInt(repeatingPart)
    val repDenominator = BigInt("9" * repeatingPart.length)
    val repFraction = Rational(repNumerator, repDenominator)
    // Apply exponent for position: -nonRepLength (not -nonRepLength - repeatingPart.length)
    val repPortion = repFraction.applyExponent(-nonRepLength)

    (intPortion + nonRepPortion + repPortion).applySign(sign)
  }
}

/**
  * Represents a vulgar (Unicode) fraction such as ⅓, ½, ¼, etc.
  * These are single Unicode characters that represent common fractions.
  *
  * @param fraction the Unicode fraction character as a string
  */
case class VulgarFractionNumber(fraction: String) extends ValuableNumber {
  /**
    * Converts the vulgar fraction Unicode character to its rational representation.
    *
    * @return `Try` containing the `Rational` equivalent of the vulgar fraction,
    *         or a failure if the character is not a recognized vulgar fraction.
    */
  def value: Try[Rational] = VulgarFraction(fraction) match {
    case Some(r) => scala.util.Success(r)
    case None => scala.util.Failure(RationalParserException(s"Unknown vulgar fraction: $fraction"))
  }
}

/**
  * A parser of Rational objects.
  */
abstract class BaseRationalParser extends SignificantSpaceParsers {

  /**
    * A "Whole" number. According to this definition all integers (Z) are whole numbers.
    * More mathematically speaking, the "whole" numbers are the Natural numbers (N) together with zero.
    *
    * @param sign   whether the parser detected a minus sign.
    * @param digits the digits read in.
    */
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

    def components: (Boolean, String, Option[String], Option[String]) = (sign, integerPart, maybeFractionalPart, exponent)
  }

  def rationalNumber: Parser[ValuableNumber] = (vulgarFraction | repeatingDecimal | realNumber | ratioNumber) :| "rationalNumber"

  /**
    * Parser for vulgar (Unicode) fractions like ⅓, ½, ¼, etc.
    * Matches any single Unicode character that represents a fraction.
    */
  def vulgarFraction: Parser[VulgarFractionNumber] = {
    // Create a regex that matches any of the vulgar fraction characters
    val fractionChars = VulgarFraction.reverseMapping.keys.mkString("|")
    val fractionRegex = s"($fractionChars)".r
    fractionRegex :| "vulgarFraction" ^^ { f => VulgarFractionNumber(f) }
  }

  def repeatingDecimal: Parser[RepeatingDecimal] =
    (opt("-") ~ unsignedWholeNumber ~ "." ~ opt(unsignedWholeNumber) ~ ("<" ~> (unsignedWholeNumber <~ ">"))) :| "repeatingDecimal" ^^ {
      case so ~ intPart ~ _ ~ nonRepOpt ~ repPart =>
        RepeatingDecimal(so.isDefined, intPart, nonRepOpt, repPart)
    }

  def ratioNumber: Parser[RatioNumber] = (simpleNumber ~ opt("/" ~> simpleNumber)) :| "ratioNumber" ^^ {
    case n ~ maybeD => RatioNumber(n, maybeD.getOrElse(WholeNumber.one))
  }

  def simpleNumber: Parser[WholeNumber] = (opt("-") ~ unsignedWholeNumber) :| "simpleNumber" ^^ {
    case so ~ n => WholeNumber(so.isDefined, n)
  }

  //  def realNumber: Parser[RealNumber] = (opt("-") ~ unsignedWholeNumber ~ ("." ~> opt(unsignedWholeNumber)) ~ opt(E ~> wholeNumber)) :| "realNumber" ^^ {

  def realNumber: Parser[RealNumber] = (opt("-") ~ unsignedWholeNumber ~ ("." ~> opt(unsignedWholeNumber)) ~ opt(E ~> ("""\+?""".r ~> wholeNumber))) :| "realNumber" ^^ {
    case so ~ integerPart ~ fractionalPart ~ expo => RealNumber(so.isDefined, integerPart, fractionalPart, expo)
  }

  /** An integer, without sign. */
  def unsignedWholeNumber: Parser[String] = logit("""\d+""".r)("unsignedWholeNumber") //^^ (x => debug(s"unsignedWholeNumber",x))

  private val E = "[eE]".r

}

/**
  * The `RationalParser` object provides functionality to parse strings into `Rational` objects.
  * It extends the `BaseRationalParser` and implements parsing logic for rational numbers,
  * including components such as sign, integer part, fractional part, and exponent.
  */
object RationalParser extends BaseRationalParser {
  /**
    * Parses the given string into a `Rational` object, if possible.
    *
    * @param s the input string to parse, representing a rational number.
    * @return a `Try[Rational]` that contains the parsed `Rational` if successful, or a failure if parsing fails.
    */
  def parse(s: String): Try[Rational] = stringParser(rationalNumber, s).flatMap(_.value)


  /**
    * Method to parse the components (sign, integerPart, maybeFractionalPart, maybeExponent) of the input string.
    *
    * @param w the String to be parsed.
    * @return a tuple of the four components.
    */
  def parseComponents(w: String): Try[(Boolean, String, Option[String], Option[String])] = parseAll(realNumber, w) match {
    case Success(p, _) => scala.util.Success(p.components)
    case Failure(z, pos) => scala.util.Failure(RationalParserException(s"cannot parse realNumber: $z, $pos"))
    case Error(z, pos) => scala.util.Failure(RationalParserException(s"cannot parse realNumber: $z, $pos"))
  }

}

/**
  * Represents an exception used specifically for handling errors related to parsing rational expressions.
  *
  * It extends the `Exception` class, allowing it to contain an error message describing the parsing issue.
  *
  * @param m The error message that provides details about the parsing error.
  */
case class RationalParserException(m: String) extends Exception(m)