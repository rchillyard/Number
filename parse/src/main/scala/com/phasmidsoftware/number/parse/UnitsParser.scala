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

  // Registry of known unit symbols
  private val unitRegistry: Map[String, Unit[?]] = Map(
    // Base SI units
    "m" -> Meter,
    "kg" -> Kilogram,
    "s" -> Second,
    "A" -> Ampere,
    "K" -> Kelvin,
    "mol" -> Mole,
    "cd" -> Candela,

    // Derived SI units
    "N" -> Newton,
    "J" -> Joule,
    "W" -> Watt,
    "Pa" -> Pascal,
    "Hz" -> Hertz,
    "C" -> Coulomb,
    "V" -> Volt,
    "Ω" -> Ohm,

    // Common non-SI length
    "km" -> Kilometer,
    "cm" -> Centimeter,
    "mm" -> Millimeter,
    "in" -> Inch,
    "ft" -> Foot,
    "yd" -> Yard,
    "mi" -> Mile,

    // Mass
    "g" -> Gram,
    "lb" -> Pound,
    "oz" -> Ounce,

    // Time
    "min" -> Minute,
    "h" -> Hour,
    "d" -> Day,

    // Angular
    "rad" -> Radian,

    // Volume
    "L" -> Liter
  )

  // Parser for a known unit symbol
  def knownUnitParser(using P[Any]): P[Unit[?]] = {
    val sortedSymbols = unitRegistry.keys.toSeq.sortBy(-_.length)

    // Try each symbol explicitly (longest first)
    sortedSymbols.foldLeft[P[Unit[?]]](P(Fail)) { (acc, sym) =>
      acc | P(sym).map(_ => unitRegistry(sym))
    }
  }

  def superscriptParser(using P[Any]): P[Int] = {
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

  // Parser for a unit with optional power
  def unitPowerParser(using P[Any]): P[Unit[?]] = {
    P(knownUnitParser ~ superscriptParser.?).map {
      case (u, None) => u
      case (u, Some(2)) => u.squared
      case (u, Some(3)) => u.cubed
      case (u, Some(-1)) => u.invert
      case (u, Some(-2)) => u.squared.invert
      case (u, Some(-3)) => u.cubed.invert
      case (u, Some(exp)) =>
        throw UnitsParserException(s"Unsupported exponent: $exp (only 2, 3, -1, -2, -3 supported)")
    }
  }

  // Parser for unit multiplication
  def unitProductParser(using P[Any]): P[Unit[?]] = {
    P(unitPowerParser ~ (("·" | "*") ~ unitPowerParser).rep).map {
      case (first, rest) => rest.foldLeft(first)(_ * _)
    }
  }

  // Parser for complete unit expression (with division)
  def unitsParser(using P[Any]): P[Unit[?]] = {
    P(unitProductParser ~ ("/" ~ unitProductParser).? ~ End).map {
      case (num, None) => num
      case (num, Some(den)) => num / den
    }
  }

  def parse(input: String): Either[String, Unit[?]] =
    fastparse.parse(input, p => unitsParser(using p)) match {
      case Parsed.Success(u, _) => Right(u)
      case f: Parsed.Failure => Left(f.trace().longMsg)
    }
}

case class UnitsParserException(msg: String) extends Exception(msg)