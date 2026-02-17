package com.phasmidsoftware.number.top

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * A sealed trait representing natural numbers (non-negative integers).
  * Natural numbers are either `Zero` or a `Successor` of another natural number.
  * This trait provides methods for basic arithmetic operations including addition, subtraction, and multiplication.
  */
sealed trait Natural:
  /**
    * Computes the next natural number by returning the successor of the current natural number.
    *
    * @return the `Successor` of this natural number.
    */
  def next: Natural

  /**
    * Adds the specified natural number to this natural number and returns the result.
    *
    * @param n the natural number to add
    * @return the result of adding the specified natural number to this one
    */
  infix def add(n: Natural): Natural

  /**
    * Subtracts the given natural number `n` from this natural number.
    * The subtraction operation is defined for natural numbers (non-negative integers).
    * If the result would be negative, the method returns `None`.
    *
    * @param n the natural number to subtract from this natural number
    * @return an `Option[Natural]` containing the result of the subtraction if non-negative, otherwise `None`
    */
  infix def subtract(n: Natural): Option[Natural] = {
    @tailrec
    def inner(r: Natural, subtrahend: Natural): Option[Natural] = (r, subtrahend) match {
      case (Zero, Zero) => Some(Zero)
      case (Zero, _) => None
      case (_, Zero) => Some(r)
      case (Successor(a), Successor(b)) => inner(a, b)
    }

    inner(this, n)
  }

  /**
    * Multiplies the current natural number by the given natural number.
    *
    * This method performs multiplication using a tail-recursive helper function. It repeatedly adds the current number
    * to an accumulator based on the value of the given `n`.
    *
    * @param n the natural number to multiply with the current number
    * @return the result of the multiplication as a natural number
    */
  infix def multiply(n: Natural): Natural = {
    @tailrec
    def inner(r: Natural, multiplicand: Natural): Natural = multiplicand match {
      case Zero =>
        r
      case Successor(natural) =>
        inner(r add n, natural)
    }

    inner(Zero, this) // Changed from 'n' to 'this'
  }

/**
  * `Zero` represents the natural number zero in the Peano arithmetic system.
  *
  * It is the base case of the natural numbers and does not have a predecessor.
  * The `Zero` object extends the `Natural` trait and provides implementations for its methods.
  *
  * - `next`: Returns the successor of zero, which is the natural number one.
  * - `add`: Returns the input natural number as the result of adding zero to it.
  * - `toString`: Overrides the string representation to an empty string.
  */
case object Zero extends Natural:
  def next: Natural = Successor(this)

  def add(n: Natural): Natural = n

  override def toString: String = ""

/**
  * Represents the successor of a natural number in the Peano arithmetic system.
  *
  * A `Successor` object encapsulates a natural number, providing various operations
  * for arithmetic and representation. It extends the `Natural` trait and defines
  * the following functionalities:
  *
  * - `next`: Returns the next natural number by creating a new `Successor` object.
  * - `add`: Recursively adds another natural number to this natural number.
  * - `toString`: Returns a Roman numeral representation of the natural number.
  *
  * @constructor Creates a `Successor` object with the given natural number.
  * @param natural A natural number representing the predecessor of this number.
  */
case class Successor(natural: Natural) extends Natural:
  def next: Natural = Successor(this)

  def add(n: Natural): Natural = {
    @tailrec
    def inner(r: Natural, addend: Natural): Natural = addend match {
      case Zero =>
        r
      case Successor(natural) =>
        inner(Successor(r), natural)
    }

    inner(this, n)
  }

  override def toString: String = {
    @tailrec
    def toRoman(acc: String, remaining: Natural, values: List[(String, Natural)]): String =
      (remaining, values) match {
        case (Zero, _) =>
          acc
        case (_, Nil) =>
          acc
        case (n, (symbol: String, value: Natural) :: rest) =>
          n subtract value match {
            case Some(diff) =>
              toRoman(acc + symbol, diff, values)
            case None =>
              toRoman(acc, n, rest)
          }
      }

    toRoman("", this, Natural.romanValues)
  }

/**
  * The `Natural` object provides utility methods for working with natural numbers
  * based on the Peano arithmetic system. It includes a method to construct a natural
  * number from an integer.
  *
  * Natural numbers are non-negative integers starting from zero. In this implementation:
  * - Zero is represented by the `Zero` object.
  * - Positive numbers are represented using the `Successor` case class, which wraps another `Natural`.
  */

import cats.Show

object Natural:
  val One: Natural = Successor(Zero)
  val Two: Natural = Successor(One)
  val Three: Natural = Successor(Two)
  val Four: Natural = Successor(Three)
  val Five: Natural = Successor(Four)
  val Six: Natural = Successor(Five)
  val Seven: Natural = Successor(Six)
  val Eight: Natural = Successor(Seven)
  val Nine: Natural = Successor(Eight)
  val Ten: Natural = Successor(Nine)
  val Forty: Natural = Ten multiply Four
  val Ninety: Natural = Ten multiply Nine
  val Fifty: Natural = Five multiply Ten
  val Hundred: Natural = Ten multiply Ten
  val FourHundred: Natural = Forty multiply Ten
  val FiveHundred: Natural = Fifty multiply Ten
  val NineHundred: Natural = Ninety multiply Ten
  val Thousand: Natural = Hundred multiply Ten
  // NOTE the following represents the largest possible Roman numeral (but not the largest Natural, of course).
  val MaxRoman: Natural = Thousand multiply Three add NineHundred add Ninety add Nine

  /**
    * A list of tuples associating Roman numeral symbols with their corresponding `Natural` values.
    * NOTE: this list must not change its order since it is used in `toString`.
    *
    * The Roman numeral system is represented as pairs of strings (symbols) and their `Natural` value equivalents.
    * The list is structured to facilitate conversion between Roman numerals and natural numbers.
    *
    * Roman numeral symbols included:
    * - "M" represents 1000 (`Thousand`)
    * - "CM" represents 900 (`NineHundred`)
    * - "D" represents 500 (`FiveHundred`)
    * - "CD" represents 400 (`FourHundred`)
    * - "C" represents 100 (`Hundred`)
    * - "XC" represents 90 (`Ninety`)
    * - "L" represents 50 (`Fifty`)
    * - "XL" represents 40 (`Forty`)
    * - "X" represents 10 (`Ten`)
    * - "IX" represents 9 (`Nine`)
    * - "V" represents 5 (`Five`)
    * - "IV" represents 4 (`Four`)
    * - "I" represents 1 (`One`)
    */
  val romanValues: List[(String, Natural)] = List(
    ("M", Thousand), ("CM", NineHundred), ("D", FiveHundred), ("CD", FourHundred),
    ("C", Hundred), ("XC", Ninety), ("L", Fifty), ("XL", Forty),
    ("X", Ten), ("IX", Nine), ("V", Five), ("IV", Four),
    ("I", One)
  )

  /**
    * A mapping of Roman numeral symbols (represented as strings) to their corresponding `Natural` values.
    *
    * This map is constructed using the `romanValues` sequence, which contains tuples of Roman numeral strings
    * and their associated `Natural` values. The additional entry `("", Zero)` is appended to ensure a mapping
    * for the empty string to the `Zero` natural number.
    *
    * Roman numeral keys in this map adhere to the conventional Roman numeral format (e.g., "I", "V", "X").
    *
    * The map can be used for parsing or validating Roman numeral strings by retrieving the `Natural` value
    * associated with a particular Roman numeral key.
    */
  val romanSymbolMap: Map[String, Natural] = (romanValues :+ ("" -> Zero)).toMap

  // Add the Show instance using given
  given Show[Natural] = Show.show(_.toString)

  /**
    * Constructs a `Natural` number from the given integer using the Peano arithmetic system.
    * The method recursively converts a non-negative integer into its corresponding `Natural` representation.
    * NOTE the purpose of this method is to make testing easier. It is not really part of the Natural API.
    *
    * @param n the integer to convert to a `Natural` number; must be non-negative
    * @return a `Natural` representation of the given integer
    * @throws IllegalArgumentException if the input integer is negative
    */
  def fromInt(n: Int): Natural = n match {
    case 0 =>
      Zero
    case n if n > 0 =>
      Successor(fromInt(n - 1))
    case _ =>
      throw new IllegalArgumentException("Natural numbers must be non-negative")
  }

/**
  * The `RomanParser` class provides functionality for parsing Roman numerals and converting them
  * into their respective natural number representations using the `Natural` type system.
  *
  * The class uses combinator parsers to validate and parse the Roman numeral strings.
  * Supported Roman numeral characters are `M`, `D`, `C`, `L`, `X`, `V`, and `I`.
  * The parser respects the rules of Roman numeral construction, including subtractive combinations like `IV` and `IX`.
  */
class RomanParser extends JavaTokenParsers:

  /**
    * Parses a given Roman numeral string and converts it to an instance of `Natural` if valid.
    *
    * If the input string does not represent a valid Roman numeral, the method returns `None`.
    *
    * @param w the Roman numeral string to parse
    * @return an `Option[Natural]` containing the parsed natural number if the input is valid,
    *         or `None` if the input is not a valid Roman numeral
    */
  def parseRoman(w: String): Option[Natural] = parseAll(romanNumeral, w) match {
    case Success(result, _) =>
      Some(result)
    case failure: NoSuccess =>
      println(failure.msg)
      None
  }

  private lazy val romanNumeral: Parser[Natural] =
    opt(thousands) ~ opt(hundreds) ~ opt(tens) ~ opt(units) ^^ {
      case t ~ h ~ te ~ u =>
        t.getOrElse(Zero) add h.getOrElse(Zero) add te.getOrElse(Zero) add u.getOrElse(Zero)
    }

  import Natural.{Thousand, Hundred, Ten, One, romanSymbolMap}

  private lazy val thousands: Parser[Natural] = "M{0,3}".r ^^ {
    m => decodeRepeated(m, Zero, Thousand)
  }
  private lazy val hundreds: Parser[Natural] = ("CM" | "CD" | "D?".r ~ "C{0,3}".r | failure("hundreds")) ^^ {
    case symbol: String =>
      romanSymbolMap(symbol)
    case symbol ~ c =>
      decodeRepeated(c, romanSymbolMap(symbol), Hundred)
  }
  private lazy val tens: Parser[Natural] = ("XC" | "XL" | "L?".r ~ "X{0,3}".r | failure("tens")) ^^ {
    case symbol: String =>
      romanSymbolMap(symbol)
    case symbol ~ x =>
      decodeRepeated(x, romanSymbolMap(symbol), Ten)
  }
  private lazy val units: Parser[Natural] = ("IX" | "IV" | "V?".r ~ "I{0,3}".r | failure("units")) ^^ {
    case symbol: String =>
      romanSymbolMap(symbol)
    case symbol ~ i =>
      decodeRepeated(i, romanSymbolMap(symbol), One)
  }

  private def decodeRepeated(x: String, identity: Natural, value: Natural): Natural =
    x.foldLeft[Natural](identity)((acc, _) => acc.add(value))

// Custom extractor for pattern matching
object Roman:
  private val parser = new RomanParser

  def unapply(s: String): Option[Natural] = parser.parseRoman(s)

  // TODO eliminate this method
  def apply(n: Natural): String = n.toString
