package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Scalar
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.expression.expr.{Expression, Literal}
import com.phasmidsoftware.number.expression.parse.NumberParser
import com.phasmidsoftware.number.parse.*

import scala.util.parsing.combinator.RegexParsers

/**
  * An object that provides utilities for parsing textual representations of quantities.
  * A quantity is composed of a numerical value, optionally followed by a physical unit.
  */
object QuantityParser extends RegexParsers with UnitsParser {

  // Disable automatic whitespace skipping — whitespace is handled explicitly.
  override val skipWhitespace: Boolean = false

  /**
    * Parses a string representation of a quantity.
    *
    * @param input the string to parse.
    * @return `Right` with the parsed `Quantity`, or `Left` with a `ParseError`.
    */
  def parse(input: String): Either[ParseError, Quantity[?]] =
    parseAll(quantityParser, input) match {
      case Success(q, _) => Right(q)
      case f: NoSuccess => Left(UnitError(f.msg))
    }

  /**
    * Parses a LaTeX command of the form `\cmd{...}{...}`.
    */
  private def latexCommand: Parser[String] =
    ("\\" ~> "[a-z]+".r) ~ rep1(braceGroup) ^^ {
      case cmd ~ groups => "\\" + cmd + groups.mkString
    }

  private def braceGroup: Parser[String] =
    "{" ~> "[^}]*".r <~ "}" ^^ (s => s"{$s}")

  private def regularNumChars: Parser[String] =
    "[0-9.+\\-eE/]+".r

  private def numberContent: Parser[String] =
    rep1(latexCommand | regularNumChars) ^^ (_.mkString)

  /**
    * Parses a numerical value using a LaTeX-compatible parser.
    */
  def numberParser: Parser[Expression] =
    numberContent ^^ doParseNumber

  /**
    * Attempts to parse `numStr` with `LaTeXParser`, falling back to `NumberParser`.
    *
    * @throws LaTeXParserException if both fail.
    */
  def doParseNumber(numStr: String): Expression =
    LaTeXParser.parse(numStr) match {
      case LaTeXParser.Success(expr, _) =>
        expr.simplify
      case _: LaTeXParser.NoSuccess =>
        NumberParser.parseNumber(numStr) match {
          case scala.util.Success(value) =>
            Literal(Scalar(value))
          case _ =>
            throw LaTeXParserException(
              s"Tried to parse '$numStr' with LaTeXParser.parse and with NumberParser.parseNumber but failed"
            )
        }
    }

  private def separator: Parser[String] = " " | "\\,"

  private def quantityParser: Parser[Quantity[?]] =
    (numberParser ~ opt(separator) ~ opt(unitsParser)) ^^ {
      case n ~ _ ~ None => Quantity(n, Dimensionless)
      case n ~ _ ~ Some(unit) => Quantity(n, unit)
    }
}