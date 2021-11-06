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
abstract class BaseMillParser extends BaseNumberParser {

  /**
    * Parse the string w as a RPN expression.
    * The elements of the input include numbers, and various operators.
    *
    * @param w the String to parse.
    * @return a Mill, wrapped in Try.
    */
  def parseMill(w: String): Try[Mill] = stringParser(mill, w.trim.split("""\s+""").reverse.mkString(" "))

  /**
    * Trait Term which represents a dyadic, monadic, anadic term (the latter including a Number).
    */
  trait Term {
    def toItems: Seq[Item] = this match {
      case AnadicTerm(x) => x match {
        case Left(w) => Seq(Item(w))
        case Right(n) => Seq(Expr(Expression(n)))
      }
      case MonadicTerm(x, os, p) => x.toItems ++ os.map(Item(_)) :+ Item(p)
      case DyadicTerm(x, p) => x.toItems ++ p.toItems
    }
  }

  /**
    * AnadicTerm is a Term defined by a Token, which is either an operator (a String) or a Number.
    *
    * @param t the defining token.
    */
  case class AnadicTerm(t: Token) extends Term {
    override def toString: String = t match {
      case Right(x) => x.toString
      case Left(x) => x
    }
  }

  /**
    * MonadicTerm is a Term defined by a Term t, a list of Strings, and an operator op.
    *
    * @param t  a Term, typically a Number or another term.
    * @param os a possibly empty list of Strings, representing neutral operators such as the swap operator.
    * @param op an monadic operator, represented by a String.
    */
  case class MonadicTerm(t: Term, os: List[String], op: String) extends Term {
    override def toString: String = s"$t $os $op"
  }

  /**
    * DyadicTerm is a Term defined by a Term t, followed by a MonadicTerm.
    *
    * @param t  a Term, typically a Number or another term.
    * @param op a MonadicTerm.
    */
  case class DyadicTerm(t: Term, op: MonadicTerm) extends Term {
    override def toString: String = s"$t $op"
  }

  /**
    * Token is something that results in a value: either
    * a Number (itself); or
    * a non-empty sequence of String each of which is an anadic (no operand) operator.
    */
  type Token = Either[String, Number]

  /**
    * A Parser[Mill].
    * This matches on a space-separated list of terms.
    *
    * @return a Parser[Mill].
    */
  def mill: Parser[Mill] = repSepSp(term) :| "mill" ^^ (items => Mill(items.flatMap(_.toItems): _*))

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
    case Left(x) => AnadicTerm(Left(x))
    case Right(w) => AnadicTerm(Right(w))
  }

  /**
    * MonadicTerm: an operator or Number which maintains the depth of the stack.
    * It matches on a monadicOperator, a list of neutral operators, followed by a term.
    *
    * @return a Parser[MonadicTerm].
    */
  def monadicTerm: Parser[MonadicTerm] = (trim(monadicOperator) ~ trim(repSepSp(neutralOperator1)) ~ term) :| "monadicTerm" ^^ {
    case y ~ os ~ x => MonadicTerm(x, os, y)
  }

  def dyadicTerm: Parser[Term] = (trim(dyadicOperator) ~ trim(repSepSp(neutralOperator2)) ~ trim(term) ~ term) :| "dyadicTerm" ^^ {
    case z ~ os ~ y ~ x => DyadicTerm(x, MonadicTerm(y, os, z))
  }

  def dyadicOperator: Parser[String] = ("+" | "*" | "×" | "^" | "-" | "−" | "–" | "/" | "÷") :| "dyadicOperator"

  def monadicOperator: Parser[String] = """(?i)chs|inv|v|ln|exp|sin|cos""".r :| "monadicOperator"

  def anadicOperator: Parser[String] = """rcl""".r :| "anadicOperator"

  def neutralOperator2: Parser[String] = ("""<>""".r | neutralOperator1) :| "neutralOperator2"

  def neutralOperator1: Parser[String] = """clr|sto""".r :| "neutralOperator1"

}

object MillParser extends BaseMillParser

