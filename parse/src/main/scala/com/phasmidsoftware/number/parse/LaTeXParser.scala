/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, Number, RationalNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.Constants
import com.phasmidsoftware.number.expression.expr.*

import scala.util.parsing.combinator.RegexParsers

/**
  * Object responsible for parsing LaTeX-style mathematical expressions
  * into an internal representation (`MathExpr`).
  */
object LaTeXParser extends RegexParsers {

  type MathExpr = com.phasmidsoftware.number.expression.expr.Expression

  // Whitespace is handled explicitly via the `ws` parser (matching the
  // original FastParse NoWhitespace behaviour).
  override val skipWhitespace: Boolean = false

  /**
    * Parses a mathematical expression from the provided input string.
    *
    * @param input the string containing the mathematical expression to be parsed
    * @return a `ParseResult[MathExpr]`; callers should match on `Success` / `NoSuccess`
    */
  def parse(input: String): ParseResult[MathExpr] =
    parseAll(parseExpr, input)

  // ── Whitespace ────────────────────────────────────────────────────────────

  private def ws: Parser[Unit] = "[ \t\n]*".r ^^^ ()

  // ── Primitives ────────────────────────────────────────────────────────────

  private def digits: Parser[String] = "[0-9]+".r

  private def scientificExponent: Parser[Int] =
    "[eE][+\\-]?[0-9]+".r ^^ (s => s.substring(1).toInt)

  private def unitSuffix: Parser[String] = "[%°]".r

  // ── Number ────────────────────────────────────────────────────────────────

  def number: Parser[MathExpr] =
    ("-".? ~ digits ~ ("." ~> digits).? ~ scientificExponent.? ~ unitSuffix.?) ^^ {
      case sign ~ intPart ~ fracPart ~ expPart ~ unitPart =>
        val signStr = sign.getOrElse("")
        val fracStr = fracPart.map(f => s".$f").getOrElse("")
        val unitStr = unitPart.getOrElse("")
        val baseNumStr = s"$signStr$intPart$fracStr"
        val numStr = expPart match {
          case Some(exp) => s"${baseNumStr}e$exp"
          case None => baseNumStr
        }
        unitStr match {
          case "%" =>
            Rational.parse(numStr).toOption match {
              case Some(r) => Literal(RationalNumber.percentage(r))
              case None => Noop(s"unable to parse LaTeX: $numStr$unitStr")
            }
          case "°" =>
            Eager(numStr) match {
              case n: Number => Literal(Angle.degrees(n))
              case _ => Noop(s"unable to parse LaTeX: $numStr$unitStr")
            }
          case "" => Eager(numStr)
        }
    }

  // ── Math symbols ──────────────────────────────────────────────────────────

  private def mathSymbol: Parser[MathExpr] =
    ("""\pi""" ^^^ Pi) |
      ("""\mathrm{\pi}""" ^^^ Pi) |
      ("\uD835\uDED1" ^^^ Pi) |
      ("π" ^^^ Pi) |
      ("""\e""" ^^^ E) |
      ("""\mathrm{e}""" ^^^ E) |
      ("""\phi""" ^^^ Root.phi) |
      ("""\mathrm{\phi}""" ^^^ Root.phi) |
      ("𝛗" ^^^ Root.phi) |
      ("""\psi""" ^^^ Root.psi) |
      ("""\mathrm{\psi}""" ^^^ Root.psi) |
      ("𝛙" ^^^ Root.psi) |
      ("\uD835\uDEFE" ^^^ Literal(Eager(Constants.gamma))) |
      ("½" ^^^ Half) |
      ("∞" ^^^ Infinity) |
      ("i" ^^^ I) // imaginary unit — must be last to avoid prefix conflicts

  // ── Fraction ──────────────────────────────────────────────────────────────

  def frac: Parser[MathExpr] =
    ("""\frac""" ~> ws ~> ("{" ~> ws ~> expr <~ ws <~ "}") ~
      (ws ~> "{" ~> ws ~> expr <~ ws <~ "}")) ^^ {
      case num ~ den => BiFunction(num, UniFunction(den, Reciprocal), Product)
    }

  // ── Square root ───────────────────────────────────────────────────────────

  def sqrt: Parser[MathExpr] =
    (("""\sqrt""" ~> ws ~> "{" ~> ws ~> expr <~ ws <~ "}") |
      ("√" ~> ws ~> atom)) ^^ { arg =>
      BiFunction(arg, Literal(Rational.half), Power)
    }

  // ── Power ─────────────────────────────────────────────────────────────────

  private def power: Parser[MathExpr] =
    (unary ~ (ws ~> ("^" | "∧") ~> ws ~> powerExponent).?) ^^ {
      case base ~ Some(exp) =>
        BiFunction(base, exp, Power)
      case base ~ None =>
        base
    }

  private def powerExponent: Parser[MathExpr] =
    ("{" ~> ws ~> expr <~ ws <~ "}") | atom

  // ── Unary ─────────────────────────────────────────────────────────────────

  private def unary: Parser[MathExpr] =
    ("-" ~> atom ^^ (a => UniFunction(a, Negate))) | atom

  // ── Functions ─────────────────────────────────────────────────────────────

  private val tupleToFunctionExpression: ((String ~ MathExpr)) => MathExpr = {
    case "sin" ~ arg => UniFunction(trigTransform(arg), Sine)
    case "cos" ~ arg => UniFunction(trigTransform(arg), Cosine)
    case "tan" ~ arg =>
      BiFunction(
        UniFunction(trigTransform(arg), Sine),
        UniFunction(UniFunction(trigTransform(arg), Cosine), Reciprocal),
        Product
      )
    case "ln" ~ arg => UniFunction(arg, Ln)
    case "exp" ~ arg => UniFunction(arg, Exp)
    case "rec" ~ arg => UniFunction(arg, Reciprocal)
    case "neg" ~ arg => UniFunction(arg, Negate)
    case "sinh" ~ arg => UniFunction(arg, Sinh)
    case "cosh" ~ arg => UniFunction(arg, Cosh)
    case x ~ _ => throw LaTeXParserException(s"unknown function: $x")
  }

  private def function: Parser[MathExpr] = {
    val fnName: Parser[String] =
      "sinh" | "cosh" | "sin" | "cos" | "tan" | "ln" | "exp" | "rec" | "neg"
    val fnArg: Parser[MathExpr] =
      ("{" ~> ws ~> expr <~ ws <~ "}") | atom
    ("\\" ~> fnName ~ (ws ~> fnArg)) ^^ tupleToFunctionExpression
  }

  // ── Atoms ─────────────────────────────────────────────────────────────────

  private def atom: Parser[MathExpr] = number | symbolicAtom

  private def symbolicAtom: Parser[MathExpr] =
    function |
      mathSymbol |
      frac |
      sqrt |
      ("(" ~> ws ~> expr <~ ws <~ ")")

  // ── Implicit multiplication ───────────────────────────────────────────────

  private def implicitMul: Parser[MathExpr] =
    (power ~ (symbolicAtom ~ (ws ~> ("^" | "∧") ~> ws ~> powerExponent).?).*) ^^ {
      case first ~ rest =>
        if (rest.isEmpty) first
        else (first +: rest.map {
          case base ~ Some(exp) => BiFunction(base, exp, Power)
          case base ~ None => base
        }).reduce((a, b) => BiFunction(a, b, Product))
    }

  // ── Term (explicit * and /) ───────────────────────────────────────────────

  private def term: Parser[MathExpr] =
    (implicitMul ~ rep((ws ~> ("*" | "/")) ~ (ws ~> implicitMul))) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "*" ~ right) => BiFunction(left, right, Product)
          case (left, "/" ~ right) => BiFunction(left, UniFunction(right, Reciprocal), Product)
          case (x, op ~ z) => throw LaTeXParserException(s"unexpected operator: $x $op $z")
        }
    }

  // ── Expression (+ and -) ─────────────────────────────────────────────────

  def expr: Parser[MathExpr] =
    (term ~ rep((ws ~> ("+" | "-")) ~ (ws ~> term))) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (left, "+" ~ right) => BiFunction(left, right, Sum)
          case (left, "-" ~ right) => BiFunction(left, UniFunction(right, Negate), Sum)
          case (x, op ~ z) => throw LaTeXParserException(s"unexpected operator: $x $op $z")
        }
    }

  // ── Top-level ─────────────────────────────────────────────────────────────

  // parseAll (called from `parse`) consumes all input, so no explicit End needed.
  private def parseExpr: Parser[MathExpr] = expr

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def trigTransform(atom: MathExpr): MathExpr = atom match {
    case Zero => Literal(Angle.zero)
    case x => x
  }
}

/**
  * This exception is thrown when an error occurs while parsing LaTeX input.
  *
  * @param message Descriptive message providing details about the exception.
  */
case class LaTeXParserException(message: String) extends RuntimeException(message)