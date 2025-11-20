/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.*
import fastparse.*
import fastparse.NoWhitespace.*

object LaTeXParser {

  type MathExpr = com.phasmidsoftware.number.expression.expr.Expression

  // Basic building blocks
  def ws[X: P]: P[Unit] = P(CharsWhileIn(" \t\n", 0))

  def digit[X: P]: P[Unit] = P(CharIn("0-9"))

  def digits[X: P]: P[String] = P(CharsWhileIn("0-9")).!

//  def number[X: P]: P[MathExpr] =
//    P(digits ~ ("." ~ digits).?).!.map(s => Literal(Eager(s)))

  def number[X: P]: P[MathExpr] =
    P(digits ~ ("." ~ digits).?).!.map(s => Expression(s))

//  def identifier[X: P]: P[String] =
//    P(CharIn("a-zA-Z")).!

//  def variable[X: P]: P[MathExpr] =
//    identifier.map(Variable(_))

  // Greek letters
  def greekLetter[X: P]: P[MathExpr] = P(
    ("""\pi""" | """\mathrm{\pi}""").!.map(_ => ConstPi) //|
//        "\\theta".map(_ => Literal(Eager("θ"))) |
//        "\\alpha".map(_ => Literal(Eager("α")))
  )

  def mathSymbol[X: P]: P[MathExpr] = P(
    ("""\pi""" | """\mathrm{\pi}""").!.map(_ => ConstPi) |
        ("""\e""" | """\mathrm{e}""").!.map(_ => ConstE)
  )

  // LaTeX commands
  def frac[X: P]: P[MathExpr] =
    P("""\frac""" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}").map {
      case (num, den) => BiFunction(num, UniFunction(den, Reciprocal), Product)
    }

  def sqrt[X: P]: P[MathExpr] =
    P("""\sqrt""" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}").map(arg =>
      BiFunction(arg, Literal(Rational.half), Power)
    )

  def power[X: P]: P[MathExpr] =
    P(atom ~ (ws ~ "^" ~ ws ~ exponent).?).map {
      case (base, Some(exp)) => BiFunction(base, exp, Power)
      case (base, None) => base
    }

  def exponent[X: P]: P[MathExpr] =
    P("{" ~ ws ~ expr ~ ws ~ "}" | atom)

  // Function calls
  def function[X: P]: P[MathExpr] = P(
    "\\" ~ (
        "sin" | "cos" | "tan" | "ln" | "exp"
        ).! ~ ws ~ (("{" ~ ws ~ expr ~ ws ~ "}") | atom)
  ).map {
    case ("sin", arg) => UniFunction(arg, Sine)
    case ("cos", arg) => UniFunction(arg, Cosine)
    case ("tan", arg) => BiFunction(UniFunction(arg, Sine), UniFunction(UniFunction(arg, Cosine), Reciprocal), Product)
    case ("ln", arg) => UniFunction(arg, Ln)
    case ("exp", arg) => UniFunction(arg, Exp)
  }

  // Atoms (highest precedence)
  def atom[X: P]: P[MathExpr] = P(
    number |
        greekLetter |
        mathSymbol |
        function |
        frac |
        sqrt |
//        variable |
        "(" ~ ws ~ expr ~ ws ~ ")"
  )

  // Implicit multiplication (no operator)
  def implicitMul[X: P]: P[MathExpr] = P(
    power.rep(1, sep = ws).map { terms =>
      if (terms.length == 1) terms.head
      else terms.reduce((a, b) => BiFunction(a, b, Product))
    }
  )

  // Explicit multiplication and division
  def term[X: P]: P[MathExpr] = P(
    implicitMul ~ (ws ~ ("*" | "/").! ~ ws ~ implicitMul).rep
  ).map {
    case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("*", right)) => BiFunction(left, right, Product)
        case (left, ("/", right)) => BiFunction(left, UniFunction(right, Reciprocal), Product)
      }
  }

  // Addition and subtraction
  def expr[X: P]: P[MathExpr] = P(
    term ~ (ws ~ ("+" | "-").! ~ ws ~ term).rep
  ).map {
    case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("+", right)) => BiFunction(left, right, Sum)
        case (left, ("-", right)) => BiFunction(left, UniFunction(right, Negate), Sum)
      }
  }

  def parseExpr[X: P]: P[MathExpr] = P(expr ~ End)

  def parse(input: String): Parsed[MathExpr] =
    fastparse.parse(input, p => parseExpr(using p))
}
