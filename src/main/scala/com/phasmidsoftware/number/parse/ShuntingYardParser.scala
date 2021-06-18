package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.mill._

import scala.annotation.tailrec
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
  def parseInfix(w: String): Try[Mill] = stringParser(shuntingYard, w).flatMap(_.toMill)

  /**
    * A ShuntingYard consisting of two stacks: the values stack and the operators stack.
    *
    * @param values    a list of values and operators. The former are added directly to values.
    *                  However, operators are only added indirectly, after a closing parenthesis
    *                  (or the end of the string) is reached.
    * @param operators a list of operators which is temporarily placed here until switch is called.
    */
  case class ShuntingYard(values: Seq[Item], operators: Seq[Item]) {

    /**
      * Method to add a token to this ShuntingYard.
      *
      * @param token the token to add.
      * @return a new ShuntingYard which is the same as this but with token added.
      *         If token is an operator, it is added to the operators;
      *         If token is a number, it is added to the values;
      *         If token is a open or close parenthesis, it is handled specially.
      */
    def :+(token: InfixToken): ShuntingYard = token match {
      case InfixToken(Some(t), _) => t match {
        case Left(operator) => this :+ operator
        case Right(number) => this :+ number
      }
      case InfixToken(None, x) =>
        if (x) this :+ openParenthesis // open parenthesis
        else switch // close parenthesis
    }

    /**
      * Convert this ShuntingYard into a Mill.
      * Any remaining operators must first be switched to the values stack.
      *
      * @return a Try[Mill].
      */
    def toMill: Try[Mill] = switch match {
      case ShuntingYard(values, Nil) => Try(Mill(values: _*))
      case x => scala.util.Failure(MillException(s"toMill: logic error with switch value (usually mis-matched parentheses): $x"))
    }

    private def :+(number: Number) = ShuntingYard(Expr(number) +: values, operators)

    private def :+(operator: String) = ShuntingYard(values, Item(operator) +: operators)

    @tailrec
    private def switch: ShuntingYard = this match {
      case ShuntingYard(values, Nil) => ShuntingYard(values, Nil)
      case ShuntingYard(values, Open :: tail) => ShuntingYard(values, tail).switch
      case ShuntingYard(values, operator :: operators) => ShuntingYard(values :+ operator, operators).switch
      case _ => throw MillException("ShuntingYard.switch: logic error")
    }
  }

  object ShuntingYard {
    /**
      * Create a new, empty, ShuntingYard.
      *
      * @return a new, empty, ShuntingYard.
      */
    def apply(): ShuntingYard = ShuntingYard(Nil, Nil)

    /**
      * Create a new ShuntingYard with the tokens added.
      *
      * @param tokens a list of InfixToken.
      * @return
      */
    def apply(tokens: Seq[InfixToken]): ShuntingYard = tokens.foldLeft(ShuntingYard())(_ :+ _).switch
  }

  /**
    * An infix token which represents either a Token or an open/close parenthesis.
    *
    * @param to    an optional token.
    * @param paren the to is None, then paren is interpreted as Open (for true) and Close (for false).
    */
  case class InfixToken(to: Option[Token], paren: Boolean)

  /**
    * Parser for a shunting yard.
    * It matches on a list of infixTokens separated by white space.
    *
    * @return a Parser[ShuntingYard].
    */
  def shuntingYard: Parser[ShuntingYard] = repSepSp(infixToken) :| "shuntingYard" ^^ (tokens => ShuntingYard(tokens))

  /**
    * Parser for an InfixToken.
    * It matches on a Number, or an operator--including ( and ).
    *
    * @return a Parser[InfixToken].
    */
  def infixToken: Parser[InfixToken] = (maybeNumber ?| operator) :| "infixToken" ^^ {
    case Right(x) => InfixToken(Some(Right(x)), paren = false)
    case Left("(") => InfixToken(None, paren = true)
    case Left(")") => InfixToken(None, paren = false)
    case Left(w) => InfixToken(Some(Left(w)), paren = false)
  }

  /**
    * Parser for a String.
    * It matches on one of the operators defined in MillParser or an open or close parenthesis.
    *
    * @return Parser[String].
    */
  def operator: Parser[String] = (dyadicOperator | monadicOperator | anadicOperator | neutralOperator2 | openParenthesis | closeParenthesis) :| "operator"

  val openParenthesis: String = "("

  val closeParenthesis: String = ")"
}

