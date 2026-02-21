package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.dimensions.core.*
import fastparse.*
import fastparse.NoWhitespace.*

/**
  * Object responsible for parsing unit expressions into their corresponding
  * `Unit` representations. This supports basic SI units, common derivations,
  * unit powers, products, and divisions.
  *
  * TODO rewrite this (and even maybe the LatexParser) using Scala Parser Combinators.
  */
object UnitsParser {

  type PhysicalUnit[D <: Dimension] = com.phasmidsoftware.number.dimensions.core.Unit[D]

  /**
    * Parses the given input string into a unit of measurement or returns an error message.
    *
    * The method attempts to parse a unit expression from the input string using the defined
    * `unitsParser`. If the parsing is successful, it returns a `Right` containing the parsed
    * unit. If the parsing fails, it returns a `Left` containing a detailed error message.
    *
    * @param input the input string to parse, representing a unit expression
    * @return an `Either` where `Right` contains the parsed unit or `Left` contains the error message
    *         NOTE that because of the way the parse method works, we can't easily distinguish between parse errors and unit errors.
    */
  def parse(input: String): Either[ParseError, Unit[?]] =
    fastparse.parse(input, p => {
      given P[Any] = p

      unitsParser ~ End
    }) match {
      case Parsed.Success(u, _) => Right(u)
      case f: Parsed.Failure => Left(UnitError(f.msg))
    }

  /**
    * Parses a complete unit expression involving division.
    *
    * This method constructs a parser that recognizes and processes unit expressions
    * combining products of units (e.g., "m·s" or "kg*m") with optional division operators ("/").
    * It supports parsing both simple unit terms and more complex unit expressions
    * with a numerator and an optional denominator. The resulting unit reflects the
    * relationship described by the parsed expression.
    *
    * @param P an implicit parameter providing the parsing context
    * @return a parser that produces a unit of measurement, taking into account the division
    *         of two unit expressions if applicable
    */
  def unitsParser(using P[Any]): P[Unit[?]] = {
    P(unitProductParser ~ ("/" ~ unitProductParser).?).map { parsed =>
      val result = parsed match {
        case (num, None) => num
        case (num, Some(den)) => num / den
      }
      result
    }
  }

  /**
    * Parses known unit symbols from input, prioritizing longer symbols to ensure
    * correct matching when multiple symbols share a prefix.
    *
    * This method leverages a registry of known unit symbols to construct a parser
    * that sequentially matches against each symbol, starting with the longest.
    * If a match is found, the corresponding unit is returned; otherwise, the parser fails.
    *
    * @param P an implicit parameter providing the parsing context
    * @return a parser that produces a unit corresponding to a recognized symbol or fails if no match is found
    */
  private def knownUnitParser(using P[Any]): P[Unit[?]] = {
    val sortedSymbols = unitRegistry.keys.toSeq.sortBy(-_.length)

    // Try each symbol explicitly (longest first)
    sortedSymbols.foldLeft[P[Unit[?]]](P(Fail)) { (acc, sym) =>
      acc | P(sym).map(_ => unitRegistry(sym))
    }
  }

  /**
    * Parses superscript characters and caret-based power expressions into integer values.
    *
    * The method supports common superscripts such as ², ³, ⁻¹, ⁻², and ⁻³, translating them to
    * their respective integer values. Additionally, it parses caret (^) expressions that
    * represent powers, including negative numbers, such as ^-2 or ^3.
    *
    * @param P an implicit parameter providing the parsing context
    * @return a parser that produces the corresponding integer value of the power
    */
  private def superscriptParser(using P[Any]): P[Int] = {
    P(
      ("²" | "³" | "⁻¹" | "⁻²" | "⁻³").! |
        ("^" ~ ("-".? ~ CharsWhileIn("0-9", 1)).!)
    ).map {
      case "²" => 2
      case "³" => 3
      case "⁻¹" => -1
      case "⁻²" => -2
      case "⁻³" => -3
      case s => s.toInt
    }
  }

  /**
    * Parses a unit with an optional power expression.
    *
    * The method combines a known unit parser with a power parser to construct
    * a unit of measurement that may optionally be raised to a power. Supported powers
    * include positive and negative integers (2, 3, -1, -2, -3). If an unsupported
    * power is encountered, an exception is thrown. The resulting unit is modified
    * based on the parsed power (e.g., squared, cubed, inverted).
    *
    * @param P an implicit parameter providing the parsing context
    * @return a parser that produces a unit raised to the specified power or throws an exception for invalid powers
    */
  private def unitPowerParser(using P[Any]): P[Unit[?]] = {
    P(knownUnitParser ~ superscriptParser.?).map { parsed =>
      val result = parsed match {
        case (u, None) => u
        case (u, Some(2)) => u.squared
        case (u, Some(3)) => u.cubed
        case (u, Some(-1)) => u.invert
        case (u, Some(-2)) => u.squared.invert
        case (u, Some(-3)) => u.cubed.invert
        case (u, Some(exp)) =>
          throw UnitsParserException(s"Unsupported exponent: $exp")
      }
      result
    }
  }
  /**
    * Parses a product of units, where units are combined using multiplication.
    *
    * The method constructs a parser that recognizes unit expressions joined by
    * the multiplication operators "·" or "*". It parses individual units with
    * optional powers using `unitPowerParser` and applies the multiplication
    * operation in sequence. The resulting composite unit reflects the combination
    * of all parsed units and their respective powers.
    *
    * @param P an implicit parameter providing the parsing context
    * @return a parser that produces a composite unit representing the product of
    *         all parsed units and their powers
    */
  //  private def unitProductParser(using P[Any]): P[Unit[?]] = {
  //    P(unitPowerParser ~ (("·" | "*") ~ unitPowerParser).rep).map {
  //      case (first, rest) => rest.foldLeft(first)(_ * _)
  //    }
  //  }

  /**
    * Parses a unit expression, optionally wrapped in parentheses.
    */
  private def unitTermParser(using P[Any]): P[Unit[?]] = {
    P(
      ("(" ~ unitProductParser ~ ")") | unitPowerParser
    )
  }

  /**
    * Parses a product of units, where units are combined using multiplication.
    */
  private def unitProductParser(using P[Any]): P[Unit[?]] = {
    P(unitTermParser ~ (("·" | "*") ~ unitTermParser).rep).map { parsed =>
      val result = parsed match {
        case (first, rest) => rest.foldLeft(first)(_ * _)
      }
      result
    }
  }
}

/**
  * Exception thrown to indicate an error encountered while parsing units.
  *
  * @param msg The error message describing the reason for the exception.
  */
case class UnitsParserException(msg: String) extends Exception(msg)