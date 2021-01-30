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
    parseAll(mill, w.trim.split("""\s+""").reverse.mkString(" ")) match {
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
      case MonadicTerm(x, os, p) => x.toItems ++ os.map(Item(_)) :+ Item(p)
      case DyadicTerm(x, p) => x.toItems ++ p.toItems
    }
  }

  case class AnadicTerm(t: Token) extends Term {
    override def toString: String = t match {
      case Right(x) => x.toString
      case Left(x) => x
    }
  }

  case class MonadicTerm(t: Term, os: List[String], op: String) extends Term {
    override def toString: String = s"$t $os $op"
  }

  case class DyadicTerm(t: Term, op: MonadicTerm) extends Term {
    override def toString: String = s"$t $op"
  }

  /**
    * Token is something that results in a value: either
    * a Number (itself); or
    * a non-empty sequence of String each of which is an anadic (no operand) operator.
    */
  type Token = Either[String, Number]

  def mill: Parser[Mill] = repsep(term, whiteSpace) :| "mill" ^^ (items => Mill(items.flatMap(_.toItems): _*))

  /**
    * A term is either of the form:
    * DyadicOp term term (net pop) or:
    * MonadicOp term (not unchanged) or:
    * AnadicOp (net push).
    *
    * @return a Parser[Term]
    */
  def term: Parser[Term] = (dyadicTerm | monadicTerm | anadicTerm) :| "term"

  /**
    * anadicTerm: an operator or Number which increases depth of the stack.
    *
    * @return a Parser[AnadicTerm]
    */
  def anadicTerm: Parser[AnadicTerm] = (maybeNumber ?| anadicOperator) :| "anadicTerm" ^^ {
    case Left(x) => AnadicTerm(Right(x))
    case Right(w) => AnadicTerm(Left(w))
  }

  def monadicTerm: Parser[MonadicTerm] = (trim(monadicOperator) ~ trim(repSepSp(neutralOperator1)) ~ term) :| "monadicTerm" ^^ {
    case y ~ os ~ x => MonadicTerm(x, os, y)
  }

  def dyadicTerm: Parser[Term] = (trim(dyadicOperator) ~ trim(repSepSp(neutralOperator2)) ~ trim(term) ~ term) :| "dyadicTerm" ^^ {
    case z ~ os ~ y ~ x => DyadicTerm(x, MonadicTerm(y, os, z))
  }

  def dyadicOperator: Parser[String] = ("+" | "*" | "×" | "^" | "-") :| "dyadicOperator"

  def monadicOperator: Parser[String] = """(?i)chs|inv|v""".r :| "monadicOperator"

  def anadicOperator: Parser[String] = """rcl""".r :| "anadicOperator"

  def neutralOperator2: Parser[String] = ("""<>""".r | neutralOperator1) :| "neutralOperator2"

  def neutralOperator1: Parser[String] = """clr|sto""".r :| "neutralOperator1"

}

