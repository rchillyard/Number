package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.mill.{Expr, Item, Mill}

import scala.util.Try

/**
  * Parser for a Mill.
  * The input to this parser is a string of tokens, each separated by any amount of white space.
  * The input is then converted to an IndexedSeq of Strings which is then reversed and rendered as a single String.
  * The purpose of this strange little dance is to reverse the order of the tokens without reversing the tokens themselves.
  *
  * CONSIDER using a TokenParser instead of a RegexParser.
  */
class MillParser extends NumberParser {

  /**
    * Parse the string w as a Number.
    * The string consists of two optional parts:
    * the numerator and the factor.
    * Either of these can be missing but not both.
    *
    * @param w the String to parse.
    * @return a Mill, wrapped in Try.
    */
  def parseMill(w: String): Try[Mill] = {
    parseAll(mill, w.split("""\s+""").reverse.mkString(" ")) match {
      case Success(t, _) => scala.util.Success(t)
      case Failure(m, _) => scala.util.Failure(RationalParserException(m))
      case Error(m, _) => scala.util.Failure(RationalParserException(m))
    }
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

  def mill: Parser[Mill] = repsep(term, whiteSpace) ^^ (items => Mill(items.flatMap(_.toItems): _*))

  def term: Parser[Term] = dyadicTerm | monadicTerm | anadicTerm

  // NOTE: currently anadic terms must be numbers. Later we will support other anadic terms.
  def anadicTerm: Parser[AnadicTerm] = number ^^ (x => AnadicTerm(Right(x)))

  def monadicTerm: Parser[MonadicTerm] = (monadicOperator <~ whiteSpace) ~ term ^^ { case y ~ x => MonadicTerm(x, y) }

  def dyadicTerm: Parser[Term] = (dyadicOperator <~ whiteSpace) ~ (term <~ whiteSpace) ~ term ^^ { case z ~ y ~ x => DyadicTerm(x, MonadicTerm(y, z)) }

  def dyadicOperator: Parser[String] = "+" | "*" | "^" | "-"

  def monadicOperator: Parser[String] = """(?i)chs|inv""".r

}

