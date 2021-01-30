package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.mill._

import scala.util.Try

/**
  * Parser for a Mill.
  * The input to this parser is a string of tokens, each separated by any amount of white space.
  * The input is then converted to an IndexedSeq of Strings which is then reversed and rendered as a single String.
  * The purpose of this strange little dance is to reverse the order of the tokens without reversing the tokens themselves.
  *
  * CONSIDER using a TokenParser instead of a RegexParser.
  */
class ShuntingYardParser extends MillParser {

  /**
    * Parse the string w as an infix expression.
    * The elements of the input include numbers, and various operators.
    *
    * @param w the String to parse.
    * @return a Mill, wrapped in Try.
    */
  def parseInfix(w: String): Try[Mill] = {
    val triedShuntingYard = parseAll(shuntingYard, w) match {
      case Success(t, _) => scala.util.Success(t)
      case Failure(m, _) => scala.util.Failure(RationalParserException(m))
      case Error(m, _) => scala.util.Failure(RationalParserException(m))
    }
    triedShuntingYard.flatMap(_.toMill)
  }

  case class ShuntingYard(values: Seq[Item], operators: Seq[Item]) {

    def :+(token: InfixToken): ShuntingYard = token match {
      case InfixToken(Some(t), _) => t match {
        case Left(operator) => this :+ operator
        case Right(number) => this :+ number
      }
      case InfixToken(None, x) =>
        if (x) this :+ openParenthesis
        else switch
    }

    def toMill: Try[Mill] = switch match {
      case ShuntingYard(values, Nil) => Try(Mill(values: _*))
      case _ => scala.util.Failure(MillException("toMill: logic error"))
    }

    private def :+(number: Number) = ShuntingYard(Expr(number) +: values, operators)

    private def :+(operator: String) = ShuntingYard(values, Item(operator) +: operators)

    private def switch = this match {
      case ShuntingYard(values, Nil) => ShuntingYard(values, Nil)
      case ShuntingYard(values, Open :: tail) => ShuntingYard(values, tail)
      case ShuntingYard(values, operator :: operators) => ShuntingYard(values :+ operator, operators)
      case _ => throw MillException("ShuntingYard.switch: logic error")
    }
  }

  object ShuntingYard {
    def apply(): ShuntingYard = ShuntingYard(Nil, Nil)

    def apply(tokens: List[InfixToken]): ShuntingYard = tokens.foldLeft(ShuntingYard())(_ :+ _).switch
  }

  case class InfixToken(to: Option[Token], paren: Boolean)

  def shuntingYard: Parser[ShuntingYard] = repSepSp(infixToken) :| "shuntingYard" ^^ (tokens => ShuntingYard(tokens))

  def infixToken: Parser[InfixToken] = (maybeNumber ?| operator) ^^ {
    case Left(x) => InfixToken(Some(Right(x)), paren = false)
    case Right("(") => InfixToken(None, paren = true)
    case Right(")") => InfixToken(None, paren = false)
    case Right(w) => InfixToken(Some(Left(w)), paren = false)
  }

  def operator: Parser[String] = (dyadicOperator | monadicOperator | anadicOperator | neutralOperator2 | openParenthesis | closeParenthesis) :| "operator"

  val openParenthesis: String = "("

  val closeParenthesis: String = ")"
}

