package com.phasmidsoftware.number.parse

/**
  * Represents an error that occurs during parsing.
  * This sealed trait serves as a base for all specific parse error types.
  */
sealed trait ParseError {
  def message: String

  def toException: ParseException = ParseException(message)
}

/**
  * Represents a syntax error encountered during parsing.
  *
  * @param message  A string describing the syntax error.
  * @param position The position in the input where the syntax error occurred.
  */
case class SyntaxError(message: String, position: Int) extends ParseError

/**
  * Represents an error related to dimensional analysis.
  *
  * @param message A string describing the dimension error.
  */
case class DimensionError(message: String) extends ParseError

/**
  * Represents an error related to unit handling.
  *
  * @param message A string describing the unit error.
  */
case class UnitError(message: String) extends ParseError

/**
  * Exception thrown when parsing fails.
  *
  * @param message Description of the parse error.
  */
case class ParseException(message: String) extends Exception(message)

extension [E <: ParseError, A](either: Either[E, A]) {
  def getOrThrow: A = either match {
    case Right(value) => value
    case Left(error) => throw new ParseException(error.message)
  }
}

