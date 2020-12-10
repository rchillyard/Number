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
      // TODO clean this up
      case None => Some(AbsoluteFuzz[Double](Rational(5).applyExponent(getExponent - realNumber.fractionalPart.length - 1).toDouble, Box))
      case Some(w) =>
        val zo: Option[Int] = w match {
          case w if w.length >= 2 => w.substring(0, 2).toIntOption
          case s => s.toIntOption map (_ * 10)
        }
        zo map (x => AbsoluteFuzz[Double](Rational(x).applyExponent(getExponent - realNumber.fractionalPart.length - 2).toDouble, Gaussian))
    }

    private def getExponent = maybeExponent.getOrElse("0").toInt
  }

  def number: Parser[Number] = opt(generalNumber) ~ opt(factor) ^^ { case ro ~ fo => optionalNumber(ro, fo).getOrElse(FuzzyNumber()) }

  def fuzz: Parser[Option[String]] = ("""\*""".r | "(" ~> """\d\d""".r <~ ")") ^^ {
    case "*" => None
    case w => Some(w)
  }

  def generalNumber: Parser[ValuableNumber] = numberWithFuzziness | rationalNumber

  def numberWithFuzziness: Parser[NumberWithFuzziness] = realNumber ~ fuzz ~ opt(rE ~> wholeNumber) ^^ { case rn ~ f ~ expo => NumberWithFuzziness(rn, f, expo) }

  private val rE = "[eE]".r

  private def optionalNumber(ro: Option[ValuableNumber], fo: Option[Factor]): Option[Number] =
    if (ro.isDefined || fo.isDefined) for (
      r <- ro.orElse(Some(WholeNumber.one));
      v <- r.value.toOption;
      f <- fo.orElse(Some(Scalar))) yield {
      val z: Option[Fuzz[Double]] = r match {
        case n@NumberWithFuzziness(_, _, _) => n.fuzz
        case _ => None
      }
      Number.apply(v, f, z)
    }
    else None

  import com.phasmidsoftware.number.core.Factor._

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) ^^ { w => Factor(w) }
}

