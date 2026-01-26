/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.expression.parse.TokenType.{DyadicOperator, MonadicOperator, Number}

import scala.util.Try
import scala.util.parsing.combinator.*

/**
  * `ExpressionParser` is a parsing utility designed for processing and interpreting arithmetic
  * expressions. It extends `JavaTokenParsers` to leverage combinator-based parsing functionality.
  *
  * CONSIDER I'm not sure that the Expression structure is that helpful in creating expressions such as BiFunction, etc.
  * Probably we should put the operator first (or maybe last).
  *
  * The parser supports parsing arithmetic expressions involving addition, subtraction,
  * multiplication, and division, as well as handling nested expressions using parentheses.
  * It is capable of transforming input strings into structured representations of expressions
  * using a robust parsing approach. The class provides detailed breakdowns of expressions
  * into components like terms, factors, numeric values, and additional constructs.
  *
  * This class also supports error handling, ensuring any invalid input is captured and reported
  * as a `Failure` within a `Try`.
  *
  * Additionally, this class offers utility methods for combining and transforming parse results,
  * and encapsulates complex parsing rules for modular expression parsing.
  *
  * Superclasses:
  * - `scala.util.parsing.combinator.JavaTokenParsers`
  * - `scala.Function1`
  * - `java.lang.Object`
  */
class ExpressionParser extends JavaTokenParsers with (String => Try[Expression]) {
  self =>

  /**
    * Applies the provided string to the parser and attempts to parse it into a result of type Expression.
    *
    * @param s the input string to be parsed
    * @return a `Try[Expression]` containing the parsed result if successful, or a failure if parsing fails
    */
  def apply(s: String): Try[Expression] =
    stringParser(expr, s).flatMap(_.value)

  /**
    * Method to parse a given String (w) such that all of it is consumed,
    * and return, if possible, a Success.
    * If the parsing fails, a Failure will be returned.
    *
    * @param p the parser to employ, of type Parser[X].
    * @param w the String to parse.
    * @tparam X the underlying type of the parser p and also of the result.
    * @return a Try[X].
    */
  def stringParser[X](p: Parser[X], w: String): Try[X] = parseAll(p, w) match {
    case Success(t, _) =>
      scala.util.Success(t)
    case Failure(m, x) =>
      scala.util.Failure(ExpressionParserException(s"""$m did not match "${x.source}" at offset ${x.offset}"""))
    case Error(m, _) =>
      scala.util.Failure(ExpressionParserException(m))
  }

  /**
    * Combines the results of two `Try` instances using a provided binary function.
    * If both `Try` instances are successful, their values are passed to the function, and the result is wrapped in a `Success`.
    * If either `Try` is a `Failure`, the resulting `Try` will also be a `Failure`.
    *
    * @param t1 the first `Try` instance
    * @param t2 the second `Try` instance
    * @param f  a binary function that takes the successful values of `t1` and `t2` and returns a new value
    * @return a `Try` containing the result of applying `f` to the values of `t1` and `t2` if both are successful,
    *         or a `Failure` if either `t1` or `t2` is a `Failure`
    */
  private def map2(t1: Try[Expression], t2: Try[Expression])(f: (Expression, Expression) => Expression): Try[Expression] =
    for tt1 <- t1; tt2 <- t2 yield f(tt1, tt2)

  trait Tokens {
    /**
      * Evaluates the expression and returns the result wrapped in a `Try`.
      * The `Try` indicates whether the evaluation was successful (`Success`) or resulted in an error (`Failure`).
      *
      * @return a `Try[Expression]` containing either the result of the evaluation or an error.
      */
    def value: Try[Expression]
  }

  /**
    * Represents a factor in an expression.
    *
    * The `Factor` class is an abstraction used to build the components of mathematical
    * or logical expressions. It serves as a fundamental unit which can be combined
    * with other components to form more complex expressions.
    *
    * This class extends the `Expression` trait, inheriting capabilities for evaluation
    * and display. Instances of `Factor` must provide concrete implementations for the
    * abstract methods defined in `Expression`.
    */
  abstract class Factor extends Tokens

  /**
    * Represents an expression consisting of a single term and a sequence of additional terms
    * combined with operators, allowing for binary operations such as addition or subtraction.
    *
    * @param t  the initial `Term` of the expression
    * @param ts a list of pairs consisting of an operator and a subsequent `Term`
    */
  case class Expr(t: Term, ts: List[String ~ Term]) extends Tokens {
    def termVal(t: String ~ Term): Try[Expression] = t match {
      case "+" ~ x =>
        x.value map (z => ExpressionToken.plus +: z)
      case "-" ~ x =>
        x.value map (z => ExpressionToken.plus +: ExpressionToken.minus +: z)
      case z ~ _ =>
        scala.util.Failure(ExpressionParserException(s"Expr: operator $z is not supported"))
    }

    /**
      * Evaluates the value of the expression represented by the `Expr` instance.
      * This method computes the result by sequentially applying the `termVal` function
      * to each term in the sequence `ts` using the operator `plus` to combine intermediary results.
      *
      * @return a `Try[Expression]` representing the result of the evaluation. If all terms are successfully
      *         evaluated, it returns a `Success` containing the final result. If an error occurs,
      *         it returns a `Failure` describing the issue.
      */
    def value: Try[Expression] =
      ts.foldLeft[Try[Expression]](t.value)((a, x) => map2(a, termVal(x))(_ + _))
  }

  /**
    * Represents a Term as part of an expression, consisting of a leading `Factor` followed
    * by a list of pairs containing an operator (`String`) and a subsequent `Factor`.
    *
    * @constructor Creates a Term with a leading `Factor` and a sequence of operator-factor pairs.
    * @param f  The leading `Factor` in the term.
    * @param fs A list of operator-factor pairs, where each pair contains a string operator and a `Factor`.
    */
  case class Term(f: Factor, fs: List[String ~ Factor]) extends Tokens {
    def value: Try[Expression] =
      fs.foldLeft[Try[Expression]](f.value)((a, x) => map2(a, factorVal(x))(_ + _))

    /**
      * Evaluates the value of a `String ~ Factor` pair based on the operator provided in the `String`.
      *
      * The function supports two operators:
      * - `"*"`: Multiplication, directly evaluates the factor's value.
      * - `"/"`: Division, applies the division operation using the `one` element and the factor's value.
      *
      * If an unsupported operator is encountered, it returns a failure with a `ParseException`.
      *
      * @param t a pair consisting of an operator (`String`) and a `Factor`
      * @return a `Try[Expression]` containing the evaluated result if the operator is supported and the factor can be evaluated,
      *         or a failure if the operator is unsupported or the evaluation fails
      */
    def factorVal(t: String ~ Factor): Try[Expression] = t match {
      case "*" ~ x =>
        x.value map (z => ExpressionToken.times +: z)
      case "/" ~ x =>
        x.value map (z => ExpressionToken.times +: ExpressionToken.rec +: z)
      case z ~ _ =>
        scala.util.Failure(ExpressionParserException(s"Term: operator $z is not supported"))
    }
  }

  /**
    * Represents a floating-point numerical factor within an expression.
    *
    * The `FloatingPoint` class models a specific type of `Factor` where the value is
    * defined by a floating-point number, represented as a string. The class provides
    * parsing capabilities to convert this string representation into an `Expression` format
    * using defined parsing logic.
    *
    * @param x the string representation of the floating-point number.
    */
  case class FloatingPoint(x: String) extends Factor {
    /**
      * Computes a parsed value derived from the `x` property of the containing instance.
      * Attempts to parse the string `x` using the provided parsing logic of `self.apply`.
      *
      * @return a `Try[Expression]` containing the parsed result if the parsing succeeds, or a `Failure` if parsing fails.
      */
    def value: Try[Expression] = Try(ExpressionTokens.create(ExpressionToken(x, Number)))
  }

  /**
    * Encapsulates an expression within parentheses. This class is used to enforce
    * precedence in mathematical or logical expressions by ensuring that the enclosed
    * expression is evaluated as a single unit.
    *
    * The `Parentheses` class extends `Factor` and wraps an `Expr` instance,
    * delegating evaluation and rendering tasks to the underlying expression.
    *
    * @param e the expression encapsulated within the parentheses
    */
  case class Parentheses(e: Expr) extends Factor {
    /**
      * Evaluates the value of the expression encapsulated by the `Parentheses` instance.
      * Delegates the evaluation to the underlying expression `e`.
      *
      * @return a `Try[Expression]` representing the evaluation result of the expression.
      *         Returns a `Success` if the evaluation completes without errors, or a `Failure` if any error occurs.
      */
    def value: Try[Expression] = e.value
  }

  /**
    * Represents a "bad factor" in an expression.
    *
    * This class is used to indicate an invalid or erroneous factor within an
    * expression. A "bad factor" effectively serves as a placeholder for scenarios
    * where parsing or constructing a valid factor is not possible. It extends
    * the `Factor` class, providing specific behavior for error handling and
    * formatted display of the associated expression.
    *
    * @param x the associated expression that led to the creation of this "bad factor".
    */
  case class BadFactor(x: Expression) extends Factor {
    /**
      * Computes the value of the factor but always results in a failure.
      * The resulting failure contains a `ParseException` with a message
      * composed of a newline followed by the formatted expression
      * representation of the factor.
      *
      * @return a `Try[Expression]`, which will always be a `Failure` containing a `ParseException`.
      */
    def value: Try[Expression] =
      scala.util.Failure(ExpressionParserException(s"\n$x\n"))
  }

  /**
    * Parses an arithmetic expression composed of terms and allows for binary operations,
    * such as addition (`+`) and subtraction (`-`), to produce an `Expr` structure.
    *
    * This parser combines a single term with a sequence of terms prefixed by operators.
    * The resulting expression can then be evaluated or further processed as needed.
    *
    * @return a `Parser[Expr]` that processes input into an `Expr` representation,
    *         encapsulating the parsed terms and their associated operators.
    */
  def expr: Parser[Expr] =
    term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case t ~ x => Expr(t, x)
    }

  /**
    * Parses a term, which consists of a `factor` followed by zero or more occurrences of either
    * multiplication or division operations, each followed by another `factor`.
    *
    * @return a `Parser[Term]` that parses a term and constructs a `Term` instance
    *         combining the initial factor and a list of operator-factor pairs.
    */
  def term: Parser[Term] =
    factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
      case f ~ r => Term(f, r)
    }

  /**
    * Parses a factor in an expression. A factor can be a number, a parenthesized expression,
    * or a failure indicating that no valid factor was present.
    *
    * @return a `Parser[Factor]` that attempts to parse and construct a valid `Factor`.
    */
  def factor: Parser[Factor] =
    number | parentheses | failure("factor")

  /**
    * Parses a numeric value, such as a floating-point number, or the mathematical constants
    * "pi" (π) and "e" (Euler's number), into a `Factor`. If no valid number or constant is
    * provided, parsing will fail.
    *
    * CONSIDER why do we need to repeat so many of the strings taken care of by the methods pi and e?
    *
    * @return a `Parser[Factor]` that converts the input into a `Factor` instance representing
    *         either the numeric value or a mathematical constant.
    */
  def number: Parser[Factor] =
    (floatingPointNumber | pi | e | failure("number")) ^^ {
      case "pi" | """𝛑""" | """\uD835\uDED1""" =>
        FloatingPoint("Pi")
      case "Euler" | "e" =>
        FloatingPoint("Euler")
      case x =>
        FloatingPoint(x)
    }

  def pi: Parser[String] = "pi" | """𝛑""" | """\uD835\uDED1""" | "π" ^^ { _ => "pi" }

  def e: Parser[String] = "e" | "\uD835\uDF00" ^^ (_ => "Euler")

  //  factor ::= "Pi" | "pi" | "PI" | 𝛑 | 𝜀 | √ | ³√

  /**
    * Parses an expression enclosed in parentheses. Ensures that the inner expression is
    * evaluated as a single unit by encapsulating it within a `Parentheses` instance.
    *
    * The parser expects an opening parenthesis `(`, followed by an expression,
    * and concludes with a closing parenthesis `)`.
    *
    * @return a parser that produces a `Parentheses` instance wrapping the parsed expression.
    */
  def parentheses: Parser[Parentheses] =
    "(" ~> expr <~ ")" ^^ (x => Parentheses(x))
}

trait Expression {
  def tokens: Seq[ExpressionToken]

  def +(expression: Expression): Expression = ExpressionTokens(tokens ++ expression.tokens)

  def +:(token: ExpressionToken): ExpressionTokens
}

object Expression {
  def apply(tokens: Seq[ExpressionToken]): Expression = ExpressionTokens(tokens)
}

case class ExpressionToken(token: String, tokenType: TokenType)

object ExpressionToken {
  val plus = ExpressionToken("+", DyadicOperator)
  val times = ExpressionToken("*", DyadicOperator)
  val minus = ExpressionToken("-", MonadicOperator)
  val rec = ExpressionToken("/", MonadicOperator)
}

case class ExpressionTokens(tokens: Seq[ExpressionToken]) extends Expression {
  def +:(token: ExpressionToken): ExpressionTokens = ExpressionTokens(token +: tokens)
}

case object EmptyExpression extends Expression {
  def tokens: Seq[ExpressionToken] = Nil

  def +:(token: ExpressionToken): ExpressionTokens = ExpressionTokens.create(token)
}

object ExpressionTokens {
  def create(tokens: ExpressionToken*) = ExpressionTokens(tokens)

  def empty: ExpressionTokens = ExpressionTokens(Seq.empty)
}

enum TokenType {
  case Number, AnadicOperator, MonadicOperator, DyadicOperator
}

case class ExpressionParserException(message: String) extends Exception(message)