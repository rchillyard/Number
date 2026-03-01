package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.dimensions.core.*

import scala.util.parsing.combinator.RegexParsers

/**
  * Object responsible for parsing unit expressions into their corresponding
  * `Unit` representations. This supports basic SI units, common derivations,
  * unit powers, products, and divisions.
  */
trait UnitsParser extends RegexParsers {
  // Disable automatic whitespace skipping — units have no spaces.
  override val skipWhitespace: Boolean = false

  type PhysicalUnit[D <: Dimension] = com.phasmidsoftware.number.dimensions.core.Unit[D]

  /**
    * Parses the given input string into a unit of measurement or returns an error.
    *
    * @param input the input string representing a unit expression
    * @return `Right` with the parsed unit, or `Left` with a `ParseError`
    */
  def parseUnit(input: String): Either[ParseError, Unit[?]] =
    parseAll(unitsParser, input) match {
      case Success(u, _) => Right(u)
      case f: NoSuccess => Left(UnitError(f.msg))
    }

  /**
    * Parses a complete unit expression involving optional division.
    * e.g. "kg·m/s²"
    */
  def unitsParser: Parser[Unit[?]] =
    (unitProductParser ~ ("/" ~> unitProductParser).?) ^^ {
      case num ~ None => num
      case num ~ Some(den) => num / den
    }

  /**
    * Tries each known unit symbol (longest first) and returns the corresponding Unit.
    */
  private def knownUnitParser: Parser[Unit[?]] = {
    val sortedSymbols = unitRegistry.keys.toSeq.sortBy(-_.length)
    sortedSymbols
      .map(sym => literal(sym) ^^^ unitRegistry(sym))
      .reduce(_ | _)
  }

  /**
    * Parses superscript characters or caret-based power expressions into integers.
    * Supports ², ³, ⁻¹, ⁻², ⁻³, and ^<integer>.
    */
  private def superscriptParser: Parser[Int] = {
    val unicodePowers: Parser[Int] =
      ("⁻³" ^^^ -3) | ("⁻²" ^^^ -2) | ("⁻¹" ^^^ -1) | ("³" ^^^ 3) | ("²" ^^^ 2)
    val caretPower: Parser[Int] =
      "^" ~> "-?[0-9]+".r ^^ (_.toInt)
    unicodePowers | caretPower
  }

  /**
    * Parses a known unit with an optional power expression.
    */
  private def unitPowerParser: Parser[Unit[?]] =
    (knownUnitParser ~ superscriptParser.?) ^^ {
      case u ~ None => u
      case u ~ Some(2) => u.squared
      case u ~ Some(3) => u.cubed
      case u ~ Some(-1) => u.invert
      case u ~ Some(-2) => u.squared.invert
      case u ~ Some(-3) => u.cubed.invert
      case u ~ Some(exp) => throw UnitsParserException(s"Unsupported exponent: $exp")
    }

  /**
    * Parses a unit expression, optionally wrapped in parentheses.
    */
  private def unitTermParser: Parser[Unit[?]] =
    ("(" ~> unitProductParser <~ ")") | unitPowerParser

  /**
    * Parses a product of units joined by "·" or "*".
    */
  private def unitProductParser: Parser[Unit[?]] =
    (unitTermParser ~ (("·" | "*") ~> unitTermParser).*) ^^ {
      case first ~ rest => rest.foldLeft(first)(_ * _)
    }
}

object UnitsParser extends UnitsParser {
  def parse(input: String): Either[ParseError, Unit[?]] = parseUnit(input)
}
/**
  * Exception thrown to indicate an error encountered while parsing units.
  *
  * @param msg The error message describing the reason for the exception.
  */
case class UnitsParserException(msg: String) extends Exception(msg)