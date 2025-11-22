/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.*
import fastparse.*
import fastparse.NoWhitespace.*

/**
  * Parses a mathematical expression, combining terms with multiplication and division operations.
  * This method recognizes explicit multiplication ("*") and division ("/") symbols between terms
  * and combines them accordingly.
  *
  * @return a parser for a sequence of mathematical expressions with multiplication and division operations.
  */
object LaTeXParser {

  type MathExpr = com.phasmidsoftware.number.expression.expr.Expression

  /**
    * Parses zero or more whitespace characters (spaces, tabs, and newlines) and consumes them in the input stream.
    * This is used to handle and ignore whitespace between different components of a LaTeX expression.
    *
    * @return a parser that matches and consumes whitespace, returning a unit value to indicate success.
    */
  def ws[X: P]: P[Unit] = P(CharsWhileIn(" \t\n", 0))

  /**
    * Parses a single-digit character (0-9).
    *
    * @return a parser that matches any single numeric character.
    */
  def digit[X: P]: P[Unit] = P(CharIn("0-9"))

  /**
    * Parses a sequence of numeric characters (digits) from the input and returns it as a string.
    *
    * The parser succeeds if it encounters one or more numeric characters.
    *
    * @return a parser that extracts a sequence of digits as a string.
    */
  def digits[X: P]: P[String] = P(CharsWhileIn("0-9")).!

  /**
    * Parses a mathematical number, which can include an optional negative sign,
    * an optional decimal point with digits before and/or after it.
    *
    * @return a `P[MathExpr]` representing the parsed mathematical expression.
    */
  def number[X: P]: P[MathExpr] =
    P(("-".? ~ digits ~ ("." ~ digits).?).!).map(s => Expression(s))

  /**
    * Parses and recognizes specific Greek letter symbols or their equivalent LaTeX representations
    * that are mapped to mathematical constants or expressions.
    *
    * @tparam X implicit evidence of a `P` context required for the parsing process.
    * @return a parser (`P`) that produces a `MathExpr` representing the corresponding constant
    *         (e.g., `ConstPi` for π) based on the recognized input.
    */
  def greekLetter[X: P]: P[MathExpr] = P(
    ("""𝛑""" | """π""").!.map(_ => ConstPi)
//        "\\theta".map(_ => Literal(Eager("θ"))) |
//        "\\alpha".map(_ => Literal(Eager("α")))
  )

  /**
    * Parses specific mathematical symbols represented as strings into their corresponding mathematical constants.
    * This method recognizes the symbols for π (pi) and e, both in plain and LaTeX-style representations,
    * and maps them to their respective case objects `ConstPi` and `ConstE`.
    *
    * @return a parser that matches strings representing mathematical constants and maps them
    *         to their corresponding `MathExpr` representations (`ConstPi` or `ConstE`).
    */
  def mathSymbol[X: P]: P[MathExpr] = P(
    ("""\pi""" | """\mathrm{\pi}""").!.map(_ => ConstPi) |
        ("""\e""" | """\mathrm{e}""").!.map(_ => ConstE) |
        "½".!.map(_ => Half) |
        "∞".!.map(_ => Infinity)
  )

  /**
    * Parses a LaTeX fraction represented by the "\frac" command.
    * This method identifies the numerator and denominator expressions
    * enclosed within curly braces and constructs a `MathExpr` representation
    * of the fraction.
    *
    * @return a parser for a mathematical fraction represented as a `MathExpr`.
    */
  def frac[X: P]: P[MathExpr] =
    P("""\frac""" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}").map {
      case (num, den) => BiFunction(num, UniFunction(den, Reciprocal), Product)
    }

  /**
    * Parses a square root expression.
    *
    * This method recognizes square root expressions in mathematical syntax,
    * such as "\sqrt{expression}" or "√atom", and parses them into appropriate
    * internal representations of square roots.
    *
    * @return A parser that produces a `MathExpr` representing the square root operation,
    *         where the operand is either an expression or an atomic value.
    */
  def sqrt[X: P]: P[MathExpr] = P(
    ("""\sqrt""" ~ ws ~ "{" ~ ws ~ expr ~ ws ~ "}") |
        ("√" ~ ws ~ atom)
  ).map(arg =>
    BiFunction(arg, Literal(Rational.half), Power)
  )

  /**
    * Parses and evaluates a mathematical expression with exponentiation.
    *
    * The method attempts to parse an expression with an optional power operator.
    * If the power operator is present, it combines the base and the exponent into a `BiFunction`,
    * representing the power operation. If absent, it returns the base expression.
    *
    * @return A parser that produces a `MathExpr` representing the parsed expression, which may
    *         include an exponentiation operation.
    */
  def power[X: P]: P[MathExpr] =
    P(unary ~ (ws ~ "^" ~ ws ~ exponent).?).map {
      case (base, Some(exp)) =>
        BiFunction(base, exp, Power)
      case (base, None) =>
        base
    }

  /**
    * Parses a unary mathematical expression, which may include a negation operation or a simple atomic expression.
    * This function identifies unary operations (e.g., negation indicated by the "-" symbol before an atomic expression)
    * and constructs a corresponding mathematical expression tree.
    *
    * @return a parsed mathematical expression (`MathExpr`) representing either a negated atomic expression
    *         or a standalone atomic mathematical entity.
    */
  def unary[X: P]: P[MathExpr] = P(
    ("-" ~ atom).map(a => UniFunction(a, Negate)) |
        atom
  )

  /**
    * Parses an exponent within a given mathematical expression. An exponent is represented as either:
    * - An expression enclosed in curly braces "{...}".
    * - A simpler mathematical atom.
    *
    * @return a parser combinator `P` that results in a `MathExpr` representing the parsed exponent.
    */
  def exponent[X: P]: P[MathExpr] =
    P("{" ~ ws ~ expr ~ ws ~ "}" | atom)

  /**
    * Parses mathematical expressions representing specific functions such as sine, cosine, tangent,
    * natural logarithm, or exponential, based on LaTeX syntax.
    *
    * The function begins by identifying a backslash `\`, followed by the function name (`sin`, `cos`, `tan`,
    * `ln`, or `exp`). It then expects whitespace and an argument, which can be encapsulated within
    * braces `{}` or represented as an atom.
    *
    * Depending on the identified function name, the appropriate mathematical operation is created:
    * - `sin`: Maps to the `Sine` operation.
    * - `cos`: Maps to the `Cosine` operation.
    * - `tan`: Maps to a combination of `Sine`, `Cosine`, and `Reciprocal`, resulting in tangent calculations.
    * - `ln`: Maps to the natural logarithm operation (`Ln`).
    * - `exp`: Maps to the exponential operation (`Exp`).
    *
    * If an unknown function name is encountered, a `LaTeXParserException` is thrown.
    *
    * @return a parsed `MathExpr` representing the function operation applied to the argument.
    */// Function calls
  def function[X: P]: P[MathExpr] = P(
    "\\" ~ (
        "sin" | "cos" | "tan" | "ln" | "exp"
        ).! ~ ws ~ (("{" ~ ws ~ expr ~ ws ~ "}") | atom)
  ).map {
    case ("sin", arg) =>
      UniFunction(arg, Sine)
    case ("cos", arg) =>
      UniFunction(arg, Cosine)
    case ("tan", arg) =>
      BiFunction(UniFunction(arg, Sine), UniFunction(UniFunction(arg, Cosine), Reciprocal), Product)
    case ("ln", arg) =>
      UniFunction(arg, Ln)
    case ("exp", arg) =>
      UniFunction(arg, Exp)
    case (x, _) =>
      throw LaTeXParserException(s"unknown function: $x")
  }

  /**
    * Parses an atomic mathematical expression. This can be a number, a mathematical function,
    * a fraction, a square root expression, a Greek letter, a mathematical symbol, or a parenthesized
    * sub-expression that itself can contain nested expressions.
    *
    * @return a parser for a mathematical expression represented by the `MathExpr` type.
    */
  def atom[X: P]: P[MathExpr] = P(
    number |
        function |
        frac |
        sqrt |
        greekLetter |
        mathSymbol |
        "(" ~ ws ~ expr ~ ws ~ ")"
  )

  /**
    * Parses a sequence of mathematical expressions separated implicitly by whitespace
    * and combines them using a multiplication operation. This implementation assumes
    * implicit multiplication when no explicit operator is provided between terms.
    *
    * The function processes a repeated pattern of terms (with at least one term)
    * derived from the `power` rule. If there is only one term, it is returned as-is.
    * If multiple terms are found, they are combined into a binary function representing
    * their product.
    *
    * @return A parser (`P[MathExpr]`) that handles implicit multiplication for a
    *         sequence of mathematical expressions.
    */// Implicit multiplication (no operator)
  def implicitMul[X: P]: P[MathExpr] = P(
    power.rep(1, sep = ws).map { terms =>
      if (terms.length == 1) terms.head
      else terms.reduce((a, b) => BiFunction(a, b, Product))
    }
  )

  /**
    * Parses and evaluates explicit multiplication and division expressions.
    * The method processes terms combined with explicit multiplication or division operators
    * (`*` or `/`), following the order of operations.
    *
    * Multiplication is represented using the `Product` operation, while division is
    * represented as multiplying the left operand by the reciprocal of the right operand.
    *
    * @return a parser that produces a `MathExpr` representing the result of evaluating
    *         multiplication and division operations on the provided terms.
    */// Explicit multiplication and division
  def term[X: P]: P[MathExpr] = P(
    implicitMul ~ (ws ~ ("*" | "/").! ~ ws ~ implicitMul).rep
  ).map {
    case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("*", right)) =>
          BiFunction(left, right, Product)
        case (left, ("/", right)) =>
          BiFunction(left, UniFunction(right, Reciprocal), Product)
        case (x, (y, z)) =>
          throw LaTeXParserException(s"unexpected operator: $x $y $z")
      }
  }

  /**
    * Parses mathematical expressions involving addition ("+") and subtraction ("-").
    *
    * This method requires an initial `term` and recursively parses multiple terms
    * separated by addition or subtraction operators. It constructs a tree-like
    * structure representing the parsed expression using binary functions (`BiFunction`).
    *
    * The operator "+" creates a `BiFunction` that adds two terms, while "-"
    * creates a `BiFunction` where the second term is negated before performing the addition.
    *
    * @return a parser (`P[MathExpr]`) that processes and builds a `MathExpr` representing
    *         a sequence of terms combined using addition and subtraction operations.
    */
  def expr[X: P]: P[MathExpr] = P(
    term ~ (ws ~ ("+" | "-").! ~ ws ~ term).rep
  ).map {
    case (first, rest) =>
      rest.foldLeft(first) {
        case (left, ("+", right)) =>
          BiFunction(left, right, Sum)
        case (left, ("-", right)) =>
          BiFunction(left, UniFunction(right, Negate), Sum)
        case (x, (y, z)) =>
          throw LaTeXParserException(s"unexpected operator: $x $y $z")
      }
  }

  /**
    * Parses a mathematical expression and ensures the input is fully consumed.
    *
    * This method combines a mathematical expression parser and an end-of-input parser.
    * The mathematical expression parser (`expr`) handles addition, subtraction, and other core constructs,
    * while the `End` validator ensures no extraneous characters remain after parsing.
    *
    * @return a `P[MathExpr]` representing a fully parsed mathematical expression.
    */
  def parseExpr[X: P]: P[MathExpr] = P(expr ~ End)

  /**
    * Parses a mathematical expression from the provided input string.
    *
    * @param input the string containing the mathematical expression to be parsed
    * @return a `Parsed[MathExpr]` result, where `MathExpr` represents the parsed expression
    *         or an error if parsing fails
    */
  def parse(input: String): Parsed[MathExpr] =
    fastparse.parse(input, p => parseExpr(using p))
}

case class LaTeXParserException(message: String) extends RuntimeException(message)