package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box, Factor, Fuzz, FuzzyNumber, Gaussian, Number, Rational, Scalar}
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
  def parseNumber(w: String): Try[Number] = parse(number, w)

  trait WithFuzziness {
    def fuzz: Option[Fuzz[Double]]
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

    def fuzz: Option[Fuzz[Double]] = fuzziness match {
        // XXX: first we deal with the "None" case
        case None => calculateFuzz(getExponent, realNumber.fractionalPart.length)
        case Some(w) =>
            val zo: Option[Int] = w match {
                case s if s.length >= 2 => s.substring(0, 2).toIntOption
                case s => s.toIntOption
            }
            zo map (x => {
                val i = getExponent - realNumber.fractionalPart.length
                AbsoluteFuzz[Double](Rational(x).applyExponent(i).toDouble, Gaussian)
            })
    }

      // CONSIDER making this a method and having two places call it
      private def getExponent = maybeExponent.getOrElse("0").toInt
  }

    def number: Parser[Number] = opt(generalNumber) ~ opt(factor) ^^ { case no ~ fo => optionalNumber(no, fo).getOrElse(FuzzyNumber()) }

    def fuzz: Parser[Option[String]] = ("""\*""".r | """\.\.\.""".r | "(" ~> """\d+""".r <~ ")") ^^ {
        case "*" => None
        case "..." => None
        case w => Some(w)
    }

    def generalNumber: Parser[ValuableNumber] = numberWithFuzziness | rationalNumber

    def numberWithFuzziness: Parser[NumberWithFuzziness] = realNumber ~ fuzz ~ opt(rE ~> wholeNumber) ^^ { case rn ~ f ~ expo => NumberWithFuzziness(rn, f, expo) }

    def exponent: Parser[String] = "[eE]".r ~> wholeNumber

    private val rE = "[eE]".r

    // NOTE: maximum length for an exact number.
    //  Any number with a longer fractional part is assumed to be fuzzy.
    val DPExact = 2

    private def optionalNumber(ro: Option[ValuableNumber], fo: Option[Factor]): Option[Number] =
        if (ro.isDefined || fo.isDefined)
            for (
                r <- ro.orElse(Some(WholeNumber.one));
                v <- r.value.toOption;
                f <- fo.orElse(Some(Scalar))) yield {
                val z: Option[Fuzz[Double]] = r match {
                    case n@NumberWithFuzziness(_, _, _) => n.fuzz
                    case n@RealNumber(_, _, Some(f), _) if f.length > DPExact => calculateFuzz(n.exponent.getOrElse("0").toInt, f.length)
                    case _ => None
                }
                Number.apply(v, f, z)
            }
        else None

    private def calculateFuzz(exponent: Int, decimalPlaces: Int): Option[Fuzz[Double]] = Some(AbsoluteFuzz[Double](Rational(5).applyExponent(exponent - decimalPlaces - 1).toDouble, Box))

    import com.phasmidsoftware.number.core.Factor._

    def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) ^^ { w => Factor(w) }
}

