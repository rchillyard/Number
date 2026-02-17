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
    def toRoman(acc: String, remaining: Natural, values: List[(Natural, String)]): String =
      (remaining, values) match {
        case (Zero, _) =>
          acc
        case (_, Nil) =>
          acc
        case (n, (value, symbol) :: rest) =>
          n subtract value match {
            case Some(diff) =>
              toRoman(acc + symbol, diff, values)
            case None =>
              toRoman(acc, n, rest)
          }
      }

    import com.phasmidsoftware.number.top.Natural.{Five, Four, Nine, One, Ten}

    val romanValues: List[(Natural, String)] = List((Ten, "X"), (Nine, "IX"), (Five, "V"), (Four, "IV"), (One, "I"))

    toRoman("", this, romanValues)
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

  // Add the Show instance using given
  given Show[Natural] = Show.show(_.toString)

  /**
    * Constructs a `Natural` number from the given integer using the Peano arithmetic system.
    * The method recursively converts a non-negative integer into its corresponding `Natural` representation.
    *
    * @param n the integer to convert to a `Natural` number; must be non-negative
    * @return a `Natural` representation of the given integer
    * @throws IllegalArgumentException if the input integer is negative
    */
  def fromInt(n: Int): Natural = n match {
    case 0 => Zero
    case n if n > 0 => Successor(fromInt(n - 1))
    case _ => throw new IllegalArgumentException("Natural numbers must be non-negative")
  }

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
    case Success(result, _) => Some(result)
    case failure: NoSuccess => None
  }

  /**
    * Parser for Roman numerals represented as a combination of thousands, hundreds, tens, and units.
    *
    * This lazy value utilizes optional sub-parsers (`thousands`, `hundreds`, `tens`, and `units`) to interpret specific parts
    * of a Roman numeral. The combined results are summed up to produce the final value as a `Natural` number.
    *
    * The parser handles partial Roman numeral strings by treating each component (thousands, hundreds, tens, and units) as optional,
    * replacing missing components with a default value of zero.
    *
    * The transformation is performed using the `^^` operator, which maps the parsed values into a `Natural` number
    * by summing up their integer equivalents and converting the total.
    *
    * @return a `Parser[Natural]` that parses a Roman numeral string and computes its corresponding `Natural` value.
    *         If no valid Roman numeral parts are found, the result defaults to zero.
    */
  lazy val romanNumeral: Parser[Natural] =
    opt(thousands) ~ opt(hundreds) ~ opt(tens) ~ opt(units) ^^ {
      case t ~ h ~ te ~ u =>
        val total = t.getOrElse(0) + h.getOrElse(0) + te.getOrElse(0) + u.getOrElse(0)
        Natural.fromInt(total)
    }

  // Thousands place
  lazy val thousands: Parser[Int] =
    "MMM" ^^^ 3000 | "MM" ^^^ 2000 | "M" ^^^ 1000

  // Hundreds place
  lazy val hundreds: Parser[Int] =
    "CM" ^^^ 900 | "DCCC" ^^^ 800 | "DCC" ^^^ 700 | "DC" ^^^ 600 |
      "D" ^^^ 500 | "CD" ^^^ 400 | "CCC" ^^^ 300 | "CC" ^^^ 200 | "C" ^^^ 100

  // Tens place
  lazy val tens: Parser[Int] =
    "XC" ^^^ 90 | "LXXX" ^^^ 80 | "LXX" ^^^ 70 | "LX" ^^^ 60 |
      "L" ^^^ 50 | "XL" ^^^ 40 | "XXX" ^^^ 30 | "XX" ^^^ 20 | "X" ^^^ 10

  // Units place
  lazy val units: Parser[Int] =
    "IX" ^^^ 9 | "VIII" ^^^ 8 | "VII" ^^^ 7 | "VI" ^^^ 6 |
      "V" ^^^ 5 | "IV" ^^^ 4 | "III" ^^^ 3 | "II" ^^^ 2 | "I" ^^^ 1

// Custom extractor for pattern matching
object Roman:
  private val parser = new RomanParser

  def unapply(s: String): Option[Natural] = parser.parseRoman(s)

  // TODO eliminate this method
  def apply(n: Natural): String = n.toString
