/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.algebra.{Angle, Eager}
import com.phasmidsoftware.number.core.Constants
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.*
import fastparse.*
import fastparse.NoWhitespace.*

/**
  * Object responsible for parsing LaTeX-style mathematical expressions
  * into an internal representation (`MathExpr`).
  */
object LaTeXParser {

  type MathExpr = com.phasmidsoftware.number.expression.expr.Expression

  /**
    * Parses a mathematical expression from the provided input string.
    *
    * @param input the string containing the mathematical expression to be parsed
    * @return a `Parsed[MathExpr]` result, where `MathExpr` represents the parsed expression
    *         or an error if parsing fails
    */
  def parse(input: String): Parsed[MathExpr] =
    fastparse.parse(input, p => parseExpr(using p))

  /**
    * Parses zero or more whitespace characters (spaces, tabs, and newlines) and consumes them in the input stream.
    * This is used to handle and ignore whitespace between different components of a LaTeX expression.
    *
    * @return a parser that matches and consumes whitespace, returning a unit value to indicate success.
    */
  private def ws[X: P]: P[Unit] = P(CharsWhileIn(" \t\n", 0))

  /**
    * Parses a sequence of numeric characters (digits) from the input and returns it as a string.
    * The parser succeeds if it encounters one or more numeric characters.
    * TODO we need to handle the "*[]" characters that specify precision here.
    * TODO we need to handle "e+-n" for scientific notation here.
    *
    * @return a parser that extracts a sequence of digits as a string.
    */
  private def digits[X: P]: P[String] = P(CharsWhileIn("0-9")).!

  /**
    * Parses a mathematical number, which can include an optional negative sign,
    * an optional decimal point with digits before and/or after it.
    *
    * @return a `P[MathExpr]` representing the parsed mathematical expression.
    */
  def number[X: P]: P[MathExpr] =
    P(("-".? ~ digits ~ ("." ~ digits).?).!).map(s => Expression(s))

  /**
    * Parses specific mathematical symbols represented as strings into their corresponding mathematical constants.
    * This method recognizes the symbols for π (pi) and e, both in plain and LaTeX-style representations,
    * and maps them to their respective case objects `ConstPi` and `ConstE`.
    *
    * @return a parser that matches strings representing mathematical constants and maps them
    *         to their corresponding `MathExpr` representations (`ConstPi` or `ConstE`).
    */
  private def mathSymbol[X: P]: P[MathExpr] = P(
    ("""\pi""" | """\mathrm{\pi}""" | "\uD835\uDED1" | "π").!.map(_ => ConstPi) |
        ("""\e""" | """\mathrm{e}""").!.map(_ => ConstE) |
        "\uD835\uDEFE".!.map(_ => Literal(Eager(Constants.gamma))) |
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
  private def power[X: P]: P[MathExpr] =
    P(unary ~ (ws ~ ("^" | "∧") ~ ws ~ exponent).?).map {
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
  private def unary[X: P]: P[MathExpr] = P(
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
  private def exponent[X: P]: P[MathExpr] =
    P("{" ~ ws ~ expr ~ ws ~ "}" | atom)

  /**
    * Transforms a tuple representing a function and its argument into a corresponding `MathExpr`.
    * The function name and the argument are used to construct a mathematical expression using
    * predefined operations such as sine, cosine, tangent, natural logarithm, exponential, reciprocal
    * and negation. Throws a `LaTeXParserException` if the provided function name is not recognized.
    *
    * Supported functions:
    * - "sin": Represents the sine function.
    * - "cos": Represents the cosine function.
    * - "tan": Represents the tangent function, constructed as sin(x) / cos(x).
    * - "ln": Represents the natural logarithm function.
    * - "exp": Represents the exponential function.
    * - "rec": Represents the reciprocal function.
    * - "neg": Represents the negation function.
    *
    * @throws LaTeXParserException if the function name is not recognized.
    */
  private val tupleToFunctionExpression: ((String, MathExpr)) => MathExpr = {
    case ("sin", arg) =>
      UniFunction(trigTransform(arg), Sine)
    case ("cos", arg) =>
      UniFunction(trigTransform(arg), Cosine)
    case ("tan", arg) =>
      BiFunction(UniFunction(trigTransform(arg), Sine), UniFunction(UniFunction(trigTransform(arg), Cosine), Reciprocal), Product)
    case ("ln", arg) =>
      UniFunction(arg, Ln)
    case ("exp", arg) =>
      UniFunction(arg, Exp)
    case ("rec", arg) =>
      UniFunction(arg, Reciprocal)
    case ("neg", arg) =>
      UniFunction(arg, Negate)
    case (x, _) =>
      throw LaTeXParserException(s"unknown function: $x")
  }

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
    */
  private def function[X: P]: P[MathExpr] = {
    P(
      "\\" ~ (
          "sin" | "cos" | "tan" | "ln" | "exp" | "rec" | "neg"
          ).! ~ ws ~ (("{" ~ ws ~ expr ~ ws ~ "}") | atom)
    ).map(tupleToFunctionExpression)
  }

  /**
    * Parses an atomic mathematical expression, which can be either a numeric value
    * or a symbolic representation.
    *
    * @return a parsed mathematical expression as an instance of MathExpr.
    */
  private def atom[X: P]: P[MathExpr] = P(number | symbolicAtom)

  /**
    * Parses a symbolic mathematical atom, which includes non-numeric entities like functions, symbols, fractions,
    * square roots, or a parenthesized expression.
    *
    * @return A parser that parses and returns a `MathExpr` representing the symbolic mathematical atom.
    */
  private def symbolicAtom[X: P]: P[MathExpr] = P(
    function |
        mathSymbol |
        frac |
        sqrt |
        "(" ~ ws ~ expr ~ ws ~ ")"
  )
  /**
    * Parses a mathematical expression with potential implicit multiplication.
    * Recognizes a sequence of elements consisting of a power expression followed
    * by zero or more symbolic atoms, and combines them with a product operation.
    *
    * @return A parser that produces a `MathExpr` representing the implicit multiplication
    *         of the parsed elements.
    */
  private def implicitMul[X: P]: P[MathExpr] = P(
    power ~ symbolicAtom.rep
  ).map { case (first, rest) =>
    if (rest.isEmpty) first
    else (first +: rest).reduce((a, b) => BiFunction(a, b, Product))
  }

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
    */
  private def term[X: P]: P[MathExpr] = P(
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
  private def parseExpr[X: P]: P[MathExpr] = P(expr ~ End)

  /**
    * Transforms a given mathematical expression into a trigonometric representation.
    *
    * @param atom The mathematical expression to be transformed. This parameter represents an
    *             instance of MathExpr that is processed to determine its trigonometric equivalent.
    * @return A Literal containing the trigonometric representation of the input expression,
    *         or the original expression if no transformation applies.
    */
  private def trigTransform(atom: MathExpr): MathExpr = atom match {
    case Zero => Literal(Angle.zero)
    case x => x
  }
}

case class LaTeXParserException(message: String) extends RuntimeException(message)