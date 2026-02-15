/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.parse

import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.*
import com.phasmidsoftware.number.core.numerical.WithFuzziness.{Asterisk, Ellipsis}

import scala.util.Try

/**
  * Parser for Number.
  *
  * NOTE when we specify an exact number by following an integer by an exponent, we must precede the "E" with a decimal point.
  * CONSIDER Try to eliminate that requirement.
  */
@deprecated("use com.phasmidsoftware.number.expression.parse.BaseNumberParser instead", "1.6.5")
abstract class BaseNumberParser extends BaseRationalParser {

  /**
    * Parse the string w as a Number.
    * The string consists of two optional parts:
    * the numerator and the factor.
    * Either of these can be missing but not both.
    *
    * @param w the String to parse.
    * @return a Number, wrapped in Try.
    */
  def parseNumber(w: String): Try[Number] =
    stringParser(number, w)

  /**
    * Parser class to represent a number with fuzziness.
    *
    * @param realNumber    a representation of a real (decimal) number in String form.
    * @param fuzziness     optional Gaussian fuzziness (as a String)--from a parenthetical number; if None, we have Box fuzziness (corresponding to "*").
    * @param maybeExponent optional exponent (as a String).
    */
  case class NumberWithFuzziness(realNumber: RealNumber, fuzziness: Option[String], maybeExponent: Option[String]) extends ValuableNumber with WithFuzziness {

    def value: Try[Rational] =
      realNumber.value.map(r => r.applyExponent(getExponent))

    def fuzz: Option[Fuzziness[Double]] = fuzziness match {
      case None => None // No fuzz marker = exact number
      case Some(Asterisk | Ellipsis) => calculateFuzz(getExponent, realNumber.fractionalPart.length) // Asterisk = box fuzz
      case Some(z) =>
        val gaussian = """\((\d*)\)""".r
        val box = """\[(\d*)]""".r
        val (shape, w) = z match {
          case gaussian(f) => (Gaussian, f)
          case box(f) => (Box, f)
          case _ => throw SignificantSpaceParserException(s"fuzziness does not match expected patterns: $z")
        }
        val zo: Option[Int] = w match {
          case s if s.length >= 2 => s.substring(0, 2).toIntOption
          case s => s.toIntOption
        }
        zo map (x => {
          val i = getExponent - realNumber.fractionalPart.length
          AbsoluteFuzz[Double](Rational(x).applyExponent(i).toDouble, shape)
        })
    }

    // CONSIDER making this a method and having two places call it
    private def getExponent =
      maybeExponent.getOrElse("0").toInt
  }

  /**
    * Parses a token representing a number. The parsing process may provide
    * a default fuzzy number if no explicit number is identified. The result
    * is tagged for clarity to indicate the type of parser.
    *
    * @return a Parser that yields a Number instance. If the parsing does not
    *         identify a concrete number, it defaults to a FuzzyNumber.
    */
  def number: Parser[Number] =
    maybeNumber :| "number" ^^ { no => no.getOrElse(FuzzyNumber()) }

  /**
    * This method defines a parser that attempts to parse an optional numerical value.
    * The parser evaluates two optional components: a general number and a factor.
    * It returns an optional `Number`, based on the combination of these parsed components.
    * The parsing process is labeled as "maybeNumber".
    *
    * @return a `Parser` that yields an `Option[Number]`.
    *         The result is `Some(Number)` if either a general number or a factor is parsed,
    *         or `None` if neither component is present.
    */
  def maybeNumber: Parser[Option[Number]] =
    (opt(generalNumber) ~ opt(factor)) :| "maybeNumber" ^^ {
      case no ~ fo => optionalNumber(no, fo)
    }

  /**
    * Method to parse fuzz, consisting of the strings "*", "...", or one or two digits enclosed in () or [].
    *
    * @return a Parser of an optional String.
    *         If the String is None, then it is general fuzz (* or ...) otherwise Some(digits).
    */
  def fuzz: Parser[Option[String]] = {
    val fuzzyDigits = """[\(\[]\d{1,2}[\)\]]""".r
    ("""\*""".r | """\.\.\.""".r | fuzzyDigits) :| "fuzz" ^^ {
      case w@Asterisk => Some(w) // Changed: return Some("*") to distinguish from ellipsis
      case w@Ellipsis => Some(w) // Changed: return Some("...") to mark as ellipsis
      case w => Some(w)
    }
  }

  /**
    * Parses either a number with fuzziness or a rational number.
    * This method combines the parsing of two distinct numeric formats:
    * - A number with an optional degree of fuzziness.
    * - A rational representation of a number.
    * The parser attempts to parse input and tag the result as `generalNumber`.
    *
    * @return a Parser that yields a ValuableNumber.
    *         The result is determined by the successful parsing of one of the numeric formats.
    */
  def generalNumber: Parser[ValuableNumber] =
    (numberWithFuzziness | rationalNumber) :| "generalNumber"

  /**
    * Parses a representation of a number that includes an optional degree of fuzziness
    * and an optional exponent. The parser expects a combination of a real number,
    * fuzziness indicator, and possibly an exponent. The fuzziness can represent
    * uncertainty or range encoded in specific formats like "*", "...", or digits in
    * parentheses or brackets.
    *
    * @return a Parser that yields a `NumberWithFuzziness` instance. The result contains
    *         the parsed real number, the optional fuzz description, and the optional
    *         exponent value.
    */
  def numberWithFuzziness: Parser[NumberWithFuzziness] =
    (realNumber ~ fuzz ~ opt(exponent)) :| "numberWithFuzziness" ^^ {
      case rn ~ f ~ expo => NumberWithFuzziness(rn, f, expo)
    }

  // NOTE: if you copy numbers from HTML, you may end up with an illegal "-" character.
  // Error message will be something like: string matching regex '-?\d+' expected but '−' found
  def exponent: Parser[String] =
    (rE ~> wholeNumber) :| "exponent"

  private val rE = "[eE]".r

  // NOTE: maximum length for an exact number. (Does this only apply to numbers presented in String form?)
  //  Any number with a longer fractional part is assumed to be fuzzy.
  private val DecimalPlacesExact = 14

  private def optionalNumber(ro: Option[ValuableNumber], fo: Option[Factor]): Option[Number] =
    if (ro.isDefined || fo.isDefined)
      for (
        r <- ro.orElse(Some(WholeNumber.one));
        v <- r.value.toOption;
        f <- fo.orElse(Some(PureNumber))) yield {
        val z: Option[Fuzziness[Double]] = r match {
          case n@NumberWithFuzziness(_, _, _) => n.fuzz
          case n@RealNumber(_, _, Some(f), _) if f.length > DecimalPlacesExact && !f.endsWith("00") => calculateFuzz(n.exponent.getOrElse("0").toInt, f.length)
          case _ => None
        }
        Number.apply(v, f, z)
      }
    else None

  /**
    * Calculates fuzziness based on the given exponent and decimal places.
    *
    * @param exponent      the exponent used in the calculation, representing the magnitude.
    * @param decimalPlaces the number of decimal places to account for in the calculation.
    * @return an `Option` containing a `Fuzziness[Double]` instance if the calculation succeeds.
    */
  private def calculateFuzz(exponent: Int, decimalPlaces: Int): Option[Fuzziness[Double]] =
    Some(AbsoluteFuzz[Double](Rational(5).applyExponent(exponent - decimalPlaces - 1).toDouble, Box))

  import Factor.*

  /**
    * Parses a factor from the input. A factor can represent specific mathematical constants,
    * symbols, or degrees. The parser attempts to match various predefined symbols such as
    * a percentage sign, degree symbol, multiple representations of π, or Euler's number.
    * If none of these are matched, the parser will fail with a defined error message.
    *
    * @return a `Parser` that produces a `Factor` instance.
    *         The result encapsulates the matched symbol or numerical value of the factor.
    */
  def factor: Parser[Factor] = (sPercent | sDegree | sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) :| "factor" ^^ { w => Factor(w) }
}

/**
  * Object for parsing numerical values, extending the functionality of the
  * `BaseNumberParser` class. Provides methods for parsing, interpreting, and
  * representing numbers, potentially with fuzziness, exponents, and other
  * modifications.
  *
  * This object specializes in defining the parsing rules for various numeric
  * representations, including:
  * - Simple numbers or rational numbers.
  * - Numbers with fuzziness or uncertainty.
  * - Factors representing constants, symbols, or mathematical concepts.
  *
  * The parsing logic accounts for optional components such as fractional parts
  * and exponents, and encapsulates results within domain-specific types, such
  * as `Number`, `NumberWithFuzziness`, and `ValuableNumber`.
  */
@deprecated("use com.phasmidsoftware.number.expression.parse.NumberParser instead", "1.6.5")
object NumberParser extends BaseNumberParser

