package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.*
import com.phasmidsoftware.number.core.FuzzyNumber.Ellipsis
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import scala.util.Try

/**
  * Parser for Number.
  *
  * NOTE when we specify an exact number by following an integer by an exponent, we must precede the "E" with a decimal point.
  * CONSIDER Try to eliminate that requirement.
  */
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
  def parseNumber(w: String): Try[Number] = stringParser(number, w)

  trait WithFuzziness {
    def fuzz: Option[Fuzziness[Double]]
  }

  /**
    * Parser class to represent a number with fuzziness.
    *
    * @param realNumber    a representation of a real (decimal) number in String form.
    * @param fuzziness     optional Gaussian fuzziness (as a String)--from a parenthetical number; if None, we have Box fuzziness (corresponding to "*").
    * @param maybeExponent optional exponent (as a String).
    */
  case class NumberWithFuzziness(realNumber: RealNumber, fuzziness: Option[String], maybeExponent: Option[String]) extends ValuableNumber with WithFuzziness {

    def value: Try[Rational] = realNumber.value.map(r => r.applyExponent(getExponent))

    def fuzz: Option[Fuzziness[Double]] = fuzziness match {
      // XXX: first we deal with the "None" case
      case None => calculateFuzz(getExponent, realNumber.fractionalPart.length)
      case Some(z) =>
        val gaussian = """\((\d*)\)""".r
        val box = """\[(\d*)]""".r
        val (shape, w) = z match {
          case gaussian(f) => (Gaussian, f)
          case box(f) => (Box, f)
          case _ => throw SignificantSpaceParserException(s"fuzziness does not match either (xx) or [xx]: $z")
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
    private def getExponent = maybeExponent.getOrElse("0").toInt
  }

  def number: Parser[Number] = maybeNumber :| "number" ^^ { no => no.getOrElse(FuzzyNumber()) }

  def maybeNumber: Parser[Option[Number]] = (opt(generalNumber) ~ opt(factor)) :| "maybeNumber" ^^ {
    case no ~ fo => optionalNumber(no, fo)
  }

  /**
    * Method to parse fuzz, consisting of the strings "*", "...", or one or two digits enclosed in () or [].
    *
    * @return a Parser of an optional String.
    *         If the String is None, then it is general fuzz (* or ...) otherwise Some(digits).
    */
  def fuzz: Parser[Option[String]] = {
    // NOTE don't take the Analyzer's suggestion to remove escapes.
    val fuzzyDigits = """[\(\[]\d{1,2}[\)\]]""".r
    ("""\*""".r | """\.\.\.""".r | fuzzyDigits) :| "fuzz" ^^ {
      case "*" => None
      // NOTE Ellipsis should indicate a quasi-exact rational number that could not be expressed exactly in decimal form.
      case Ellipsis => None
      case w => Some(w)
    }
  }

  def generalNumber: Parser[ValuableNumber] = (numberWithFuzziness | rationalNumber) :| "generalNumber"

  def numberWithFuzziness: Parser[NumberWithFuzziness] = (realNumber ~ fuzz ~ opt(exponent)) :| "numberWithFuzziness" ^^ {
    case rn ~ f ~ expo => NumberWithFuzziness(rn, f, expo)
  }

  // NOTE: if you copy numbers from HTML, you may end up with an illegal "-" character.
  // Error message will be something like: string matching regex '-?\d+' expected but '−' found
  def exponent: Parser[String] = (rE ~> wholeNumber) :| "exponent"

  private val rE = "[eE]".r

  // NOTE: maximum length for an exact number.
  //  Any number with a longer fractional part is assumed to be fuzzy.
  private val DPExact = 2

  private def optionalNumber(ro: Option[ValuableNumber], fo: Option[Factor]): Option[Number] =
    if (ro.isDefined || fo.isDefined)
      for (
        r <- ro.orElse(Some(WholeNumber.one));
        v <- r.value.toOption;
        f <- fo.orElse(Some(PureNumber))) yield {
        val z: Option[Fuzziness[Double]] = r match {
          case n@NumberWithFuzziness(_, _, _) => n.fuzz
          case n@RealNumber(_, _, Some(f), _) if f.length > DPExact && !f.endsWith("00") => calculateFuzz(n.exponent.getOrElse("0").toInt, f.length)
          case _ => None
        }
        Number.apply(v, f, z)
      }
    else None

  private def calculateFuzz(exponent: Int, decimalPlaces: Int): Option[Fuzziness[Double]] = Some(AbsoluteFuzz[Double](Rational(5).applyExponent(exponent - decimalPlaces - 1).toDouble, Box))

  import Factor.*

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) :| "factor" ^^ { w => Factor(w) }
}

object NumberParser extends BaseNumberParser

