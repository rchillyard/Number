package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.Valuable
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.parse.*
import fastparse.*
import fastparse.NoWhitespace.*

object QuantityParser {

  // Parser for numeric values using the existing LaTeX parser
  // Parser for numeric values using the existing LaTeX parser
  def numberParser(using P[Any]): P[Valuable] = {
    P(CharsWhile(c =>
      c.isDigit || c == '.' || c == '/' || c == '-' || c == '+' ||
        c == 'e' || c == 'E' || c == '\\' || c == '{' || c == '}'
    ).!).flatMap { numStr =>
      // Check if it ends with backslash (incomplete \, separator)
      if (numStr.endsWith("\\")) {
        Pass(numStr.dropRight(1))
      } else {
        Pass(numStr)
      }
    }.map { numStr =>
      LaTeXParser.parse(numStr) match {
        case fastparse.Parsed.Success(expr, _) =>
          expr.simplify.materialize
        case failure: fastparse.Parsed.Failure =>
          throw LaTeXParserException(s"Failed to parse numeric value '$numStr': ${failure.trace().longMsg}")
      }
    }
  }

  // Main quantity parser: number followed by optional whitespace and units
  def quantityParser(using P[Any]): P[Quantity[?]] = {
    P(numberParser ~ (P(" ") | P("\\,")).? ~ UnitsParser.unitsParser.? ~ End).map {
      case (n, u) => u match {
        case None => Quantity(n, Dimensionless)
        case Some(unit) => Quantity(n, unit)
      }
    }
  }

  // Parse function
  def parse(input: String): Either[ParseError, Quantity[?]] = {
    fastparse.parse(input, p => quantityParser(using p)) match {
      case Parsed.Success(q, _) => Right(q)
      case f: Parsed.Failure => Left(UnitError(f.msg))
    }
  }
}