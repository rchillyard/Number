package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.{AbsoluteFuzz, Factor, Fuzz, FuzzyNumber, Number, Rational, Scalar, Shape}

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
    def fuzz: Fuzz[Double]
  }

  case class NumberWithFuzziness(realNumber: RealNumber, fuzziness: Option[Int], maybeExponent: Option[String]) extends ValuableNumber with WithFuzziness {

    def value: Try[Rational] = realNumber.value.map(r => r.applyExponent(getExponent))

    def fuzz: Fuzz[Double] = measureFuzziness()

    private def getExponent = maybeExponent.getOrElse("0").toInt

    private def measureFuzziness() = new AbsoluteFuzz[Double] {
      override val magnitude: Double = Rational.one.applyExponent(getExponent - realNumber.fractionalPart.length - 2).toDouble
      override val shape: Shape = com.phasmidsoftware.number.core.Gaussian // TODO deal with the "*" case as a Box
    }

  }

  def number: Parser[Number] = opt(generalNumber) ~ opt(factor) ^^ { case ro ~ fo => optionalNumber(ro, fo).getOrElse(FuzzyNumber()) }

  def fuzz: Parser[Option[Int]] = ("""\*""".r | "(" ~> """\d\d""".r <~ ")") ^^ {
    case "*" => Some(50)
    case w if w.length >= 2 => w.substring(0, 2).toIntOption
    case s => s.toIntOption map (_ * 10)
  }

  def generalNumber: Parser[ValuableNumber] = numberWithFuzziness | rationalNumber

  def numberWithFuzziness: Parser[NumberWithFuzziness] = realNumber ~ fuzz ~ opt(rE ~> wholeNumber) ^^ { case rn ~ f ~ expo => NumberWithFuzziness(rn, f, expo) }

  //  def realNumber: Parser[Option[String] ~ String ~ Option[String]] = {
  //    opt("-") ~ wholeNumber ~ opt("." ~> wholeNumber)
  //  }

  private val rE = "[eE]".r

  private def optionalNumber(ro: Option[ValuableNumber], fo: Option[Factor]): Option[Number] =
    if (ro.isDefined || fo.isDefined) for (
      r <- ro.orElse(Some(WholeNumber.one));
      v <- r.value.toOption;
      f <- fo.orElse(Some(Scalar))) yield {
      val z: Option[Fuzz[Double]] = r match {
        case n@NumberWithFuzziness(_, _, _) => Some(n.fuzz)
        case _ => None
      }
      Number.apply(v, f, z)
    }
    else None

  import com.phasmidsoftware.number.core.Factor._

  def factor: Parser[Factor] = (sPi | sPiAlt0 | sPiAlt1 | sPiAlt2 | sE | failure("factor")) ^^ { w => Factor(w) }
}

