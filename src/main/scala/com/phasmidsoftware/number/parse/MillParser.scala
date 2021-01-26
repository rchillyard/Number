package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.mill.{Expr, Item, Mill}

import scala.util.Try

/**
  * Parser for Number.
  */
class MillParser extends NumberParser {

  /**
    * Parse the string w as a Number.
    * The string consists of two optional parts:
    * the numerator and the factor.
    * Either of these can be missing but not both.
    *
    * CONSIDER is it possible to simply use the super.parse method?
    *
    * @param w the String to parse.
    * @return a Mill, wrapped in Try.
    */
  def parseMill(w: String): Try[Mill] = parseAll(mill, w) match {
    case Success(t, _) => scala.util.Success(t)
    case Failure(m, _) => scala.util.Failure(RationalParserException(m))
    case Error(m, _) => scala.util.Failure(RationalParserException(m))
  }

  trait Term {
    def toItems: Seq[Item] = this match {
      case AnadicTerm(x) => x match {
        case Left(w) => Seq(Item(w))
        case Right(n) => Seq(Expr(n))
      }
      case MonadicTerm(x, op) => x.toItems :+ Item(op)
      case DyadicTerm(x, op) => x.toItems ++ op.toItems
    }
  }

  case class AnadicTerm(x: Token) extends Term

  case class MonadicTerm(x: Term, op: String) extends Term

  case class DyadicTerm(x: Term, op: MonadicTerm) extends Term

  /**
    * Token is something that results in a value: either
    * a Number (itself); or
    * a String which is an anadic (no operand) operator.
    */
  type Token = Either[String, Number]

  def mill: Parser[Mill] = repsep(term, """\s+""".r) ^^ ( items => Mill(items: _*) )

  def term: Parser[Item] = (operator | number) ^^ {
    case x: Number => Expr(x)
    case w: String => Item(w)
  }

  //  // NOTE: currently anadic terms must be numbers. Later we will support other anadic terms.
  //  def anadicTerm: Parser[AnadicTerm] = number ^^ (x => AnadicTerm(Right(x)))
  //
  //  def monadicTerm: Parser[MonadicTerm] = term ~ monadicOperator ^^ {case x ~ y => MonadicTerm(x, y)}
  //
  //  def dyadicTerm: Parser[Term] = term ~ term ~ dyadicOperator ^^ { case x ~ y ~ z => DyadicTerm(x, MonadicTerm(y, z)) }

  private def operator = dyadicOperator | monadicOperator

  private def dyadicOperator = "+" | "*" | "^"

  private def monadicOperator = """(?i)chs|inv""".r

}

