package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Scalar
import com.phasmidsoftware.number.core.parse.NumberParser
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.expression.expr.{Expression, Literal}
import com.phasmidsoftware.number.parse.*
import fastparse.*
import fastparse.NoWhitespace.*

/**
  * An object that provides utilities for parsing textual representations of quantities.
  * A quantity is composed of a numerical value, optionally followed by a physical unit.
  * The object contains methods and parsers to handle numeric and unit parsing, with support
  * for common formats including LaTeX-compatible syntax.
  */
object QuantityParser {

  /**
    * Parses a string representation of a quantity, which consists of a numerical value
    * optionally followed by a physical unit. The input string is processed using the `quantityParser`.
    *
    * @param input the string to parse. It may include a numerical value, followed by an optional
    *              unit specification separated by whitespace or a comma.
    *
    * @return an `Either` containing a successfully parsed `Quantity` wrapped in `Right`, or
    *         a `ParseError` wrapped in `Left` if the parsing fails.
    */
  def parse(input: String): Either[ParseError, Quantity[?]] = {
    fastparse.parse(input, p => quantityParser(using p)) match {
      case Parsed.Success(q, _) => Right(q)
      case f: Parsed.Failure => Left(UnitError(f.msg))
    }
  }

  /**
    * Parses a string representation of a numerical value using a LaTeX-compatible parser.
    * The parser processes characters commonly used in numerical expressions, including
    * digits, decimal points, fractions, scientific notation, and LaTeX syntax elements.
    *
    * @param P an implicit parameter representing the parsing context, which defines
    *          the utilities and environment used for the parser.
    *
    * @return a parser that produces an `Expression` representation of the parsed numerical value.
    */
  def numberParser(using P[Any]): P[Expression] =
    P(CharsWhile(c =>
      c.isDigit || c == '.' || c == '/' || c == '-' || c == '+' ||
        c == 'e' || c == 'E' || c == '\\' || c == '{' || c == '}'
    ).!).flatMap { numStr =>
      // Check if it ends with backslash (incomplete \, separator)
      if (numStr.endsWith("\\"))
        Pass(numStr.dropRight(1))
      else
        Pass(numStr)
    }.map(doParseNumber)

  /**
    * Parses a string representation of a number, attempting to process it first using
    * the LaTeX parser. If the LaTeX parser fails, it falls back to a numeric parser.
    * If both attempts fail, an exception is thrown.
    *
    * @param numStr the string to parse, expected to represent a number
    * @return an `Expression` object, either parsed and simplified using the LaTeX parser
    *         or constructed as a literal scalar value in case of numeric parsing
    *
    * @throws LaTeXParserException if the input cannot be parsed using either parser
    */
  def doParseNumber(numStr: String): Expression =
    LaTeXParser.parse(numStr) match {
      case fastparse.Parsed.Success(expr, _) =>
        expr.simplify
      case failure: Parsed.Failure =>
        NumberParser.parseNumber(numStr) match {
          case scala.util.Success(value) =>
            Literal(Scalar(value))
          case _ =>
            throw LaTeXParserException(s"Tried to parse '$numStr' with LaTeXParser.parse and with NumberParser.parseNumber but failed")
        }
    }

  /**
    * Parses a textual representation of a quantity, consisting of a numerical value
    * optionally followed by a unit. Supports optional whitespace or a comma character
    * as a separator between the numerical value and the unit.
    *
    * @param P an implicit parameter representing the context for the parser framework.
    *          It defines the parsing environment and utilities used during parsing.
    *
    * @return a parser that produces a `Quantity` object. The resulting quantity has a numerical
    *         value and an optional physical unit. If no unit is provided in the input,
    *         the quantity defaults to a dimensionless unit.
    */
  private def quantityParser(using P[Any]): P[Quantity[?]] = {
    P(numberParser ~ (P(" ") | P("\\,")).? ~ UnitsParser.unitsParser.? ~ End).map {
      case (n, u) => u match {
        case None => Quantity(n, Dimensionless)
        case Some(unit) => Quantity(n, unit)
      }
    }
  }
}