/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import scala.util.Try
import scala.util.parsing.combinator.*

/**
  * @author scalaprof
  */
abstract class ExpressionParser[T] extends JavaTokenParsers with (String => Try[T]) {
  self =>

  /**
    * Applies the provided string to the parser and attempts to parse it into a result of type T.
    *
    * @param s the input string to be parsed
    * @return a `Try[T]` containing the parsed result if successful, or a failure if parsing fails
    */
  def apply(s: String): Try[T]

  /**
    * Method representing the division operation.
    * This method is a higher-order function that accepts two parameters of type `T` and returns a result of type `T`.
    * It encapsulates the logic for dividing one value of type `T` by another.
    *
    * @return a function of type `(T, T) => T` which performs division on two inputs of type `T` and produces a result of type `T`.
    */
  def div: (T, T) => T

  /**
    * Represents a unary operation that negates a given value.
    *
    * @return a function that takes a value of type T and returns its negation.
    */
  def negate: T => T

  /**
    * Combines two values of the same type and produces a new value of the same type using the defined operation.
    *
    * @return A function that takes two arguments of type T and returns a result of type T.
    */
  def plus: (T, T) => T

  /**
    * A function representing the multiplication operation.
    *
    * @return a binary function that takes two arguments of type `T`
    *         and returns their product of type `T`.
    */
  def times: (T, T) => T

  /**
    * Retrieves an instance of type T.
    *
    * @return an instance of type T
    */
  def one: T

  /**
    * Returns the zero value for the type `T`. The zero value is typically defined as the additive identity in a mathematical context.
    *
    * @return The zero value of type `T`.
    */
  def zero: T

  /**
    * Represents an abstract mathematical or logical expression.
    *
    * The `Expression` trait provides a contract for evaluating and displaying expressions.
    * It is designed to handle expressions that return a result of type `T` wrapped in a `Try`,
    * allowing for both successful evaluations and error reporting.
    */
  trait Expression {
    /**
      * Evaluates the expression and returns the result wrapped in a `Try`.
      * The `Try` indicates whether the evaluation was successful (`Success`) or resulted in an error (`Failure`).
      *
      * @return a `Try[T]` containing either the result of the evaluation or an error.
      */
    def value: Try[T]

    /**
      * Displays a textual representation of the expression with the specified indentation.
      *
      * @param indent the number of spaces to prepend to each line of the output, used to control indentation.
      * @return a string representing the formatted expression.
      */
    def show(indent: Int): String
  }

  /**
    * Transforms the value inside the provided `Try[T]` instance using the given function.
    *
    * @param t a `Try[T]` containing either a successful value of type `T` or a failure
    * @param f a function that takes a value of type `T` and returns a transformed value of type `T`
    * @return a `Try[T]` containing the transformed value if `t` is successful, or the original failure if `t` is a failure
    */
  def lift(t: Try[T])(f: T => T): Try[T] = t map f

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
  def map2(t1: Try[T], t2: Try[T])(f: (T, T) => T): Try[T] =
    for tt1 <- t1; tt2 <- t2 yield f(tt1, tt2)

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
  abstract class Factor extends Expression

  /**
    * Represents an expression consisting of a single term and a sequence of additional terms
    * combined with operators, allowing for binary operations such as addition or subtraction.
    *
    * @param t  the initial `Term` of the expression
    * @param ts a list of pairs consisting of an operator and a subsequent `Term`
    */
  case class Expr(t: Term, ts: List[String ~ Term]) extends Expression {
    def termVal(t: String ~ Term): Try[T] = t match {
      case "+" ~ x =>
        x.value;
      case "-" ~ x =>
        lift(x.value)(negate);
      case z ~ _ =>
        scala.util.Failure(ParseException(s"Expr: operator $z is not supported"))
    }

    /**
      * Evaluates the value of the expression represented by the `Expr` instance.
      * This method computes the result by sequentially applying the `termVal` function
      * to each term in the sequence `ts` using the operator `plus` to combine intermediary results.
      *
      * @return a `Try[T]` representing the result of the evaluation. If all terms are successfully
      *         evaluated, it returns a `Success` containing the final result. If an error occurs,
      *         it returns a `Failure` describing the issue.
      */
    def value: Try[T] =
      ts.foldLeft(t.value)((a, x) => map2(a, termVal(x))(plus))

    /**
      * Formats a string representation of a term, combining its operator and term at the given indentation level.
      *
      * @param t a tuple consisting of a `String` representing an operator and a `Term`
      * @param i the current indentation level
      * @return a formatted `String` representation of the term with proper indentation
      */
    def termShow(t: String ~ Term, i: Int): String =
      s"${t._1}${new_line(i)}${t._2.show(i + 1)}"

    /**
      * Generates a string representation of the expression starting from the specified indentation level.
      *
      * This method formats the current expression including its terms and subterms indented by the given level.
      *
      * @param i the indentation level used to format the output
      * @return a string representation of the expression with appropriate indentation and structure
      */
    def show(i: Int): String = {
      val sb = new StringBuilder("Expr: " + new_line(i + 1) + t.show(i + 1))
      if ts.nonEmpty then {
        sb.append(ts.foldLeft(" {")((a, x) => a + new_line(i + 1) + termShow(x, i + 1)))
        sb.append(new_line(i + 1) + "}")
      }
      sb.toString
    }
  }

  /**
    * Represents a Term as part of an expression, consisting of a leading `Factor` followed
    * by a list of pairs containing an operator (`String`) and a subsequent `Factor`.
    *
    * @constructor Creates a Term with a leading `Factor` and a sequence of operator-factor pairs.
    * @param f  The leading `Factor` in the term.
    * @param fs A list of operator-factor pairs, where each pair contains a string operator and a `Factor`.
    */
  case class Term(f: Factor, fs: List[String ~ Factor]) extends Expression {
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
      * @return a `Try[T]` containing the evaluated result if the operator is supported and the factor can be evaluated,
      *         or a failure if the operator is unsupported or the evaluation fails
      */
    def factorVal(t: String ~ Factor): Try[T] = t match {
      case "*" ~ x =>
        x.value;
      case "/" ~ x =>
        map2(Try(one), x.value)(div);
      case z ~ _ =>
        scala.util.Failure(ParseException(s"Term: operator $z is not supported"))
    }

    /**
      * Computes the result of folding a sequence of inputs using the given operations `map2` and `times`.
      * This method starts with the initial value `f.value` and iteratively combines it with
      * each result of applying `factorVal` on elements of `fs`.
      *
      * @return A `Try[T]` representing the result of the fold operation or the encountered failure.
      */
    def value: Try[T] =
      fs.foldLeft(f.value)((a, x) => map2(a, factorVal(x))(times))

    /**
      * Combines the string representation of a tuple containing a string and a Factor,
      * formatting it with a newline and the provided indentation level.
      *
      * @param t A tuple containing a string (`t._1`) and a `Factor` (`t._2`).
      * @param i The indentation level to be used for formatting.
      * @return A formatted string that combines the first element of the tuple with
      *         the string representation of the Factor, separated by a newline,
      *         with the appropriate indentation.
      */
    def factorShow(t: String ~ Factor, i: Int): String =
      s"${t._1}${new_line(i)}${t._2.show(i + 1)}"

    /**
      * Constructs and returns a formatted string representation
      * of the term and its components based on the provided input.
      *
      * @param i the integer input used to determine the formatting and content of the output string
      * @return a formatted string representation of the term and its components
      */
    def show(i: Int): String = {
      val sb = new StringBuilder("Term: " + new_line(i + 1) + f.show(i + 1))
      if fs.nonEmpty then {
        sb.append(fs.foldLeft(" {")((a, x) => a + new_line(i + 1) + factorShow(x, i + 1)))
        sb.append(new_line(i + 1) + "}")
      }
      sb.toString
    }
  }

  /**
    * The `FloatingPoint` case class extends the `Factor` abstraction and represents
    * a factor that holds a floating-point value in string representation.
    *
    * @param x the string value representing a floating-point number to be parsed and utilized.
    */
  case class FloatingPoint(x: String) extends Factor {
    /**
      * Computes a parsed value derived from the `x` property of the containing instance.
      * Attempts to parse the string `x` using the provided parsing logic of `self.apply`.
      *
      * @return a `Try[T]` containing the parsed result if the parsing succeeds, or a `Failure` if parsing fails.
      */
    def value: Try[T] = self.apply(x)

    /**
      * Returns a string representation of the class that combines a constant prefix
      * with the current instance's `x` value.
      *
      * @param i an integer input (not directly used in this implementation, but retained for method signature structure).
      * @return a string in the format "GeneralNumber: " followed by the `x` value of the instance.
      */
    def show(i: Int): String = "GeneralNumber: " + x
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
      * @return a `Try[T]` representing the evaluation result of the expression.
      *         Returns a `Success` if the evaluation completes without errors, or a `Failure` if any error occurs.
      */
    def value: Try[T] = e.value

    /**
      * Generates a string representation of the expression enclosed in parentheses at the specified indentation level.
      * This method formats the output with line breaks and indentation,
      * using the result of `e.show` recursively with an increased indentation level.
      *
      * @param i the indentation level applied to format the output
      * @return a string representation of the expression enclosed in parentheses,
      *         formatted with appropriate line breaks and indentation
      */
    def show(i: Int): String =
      "(" + new_line(i) + e.show(i + 1) + ")"
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
      * @return a `Try[T]`, which will always be a `Failure` containing a `ParseException`.
      */
    def value: Try[T] =
      scala.util.Failure(ParseException("\n" + x.show(0) + "\n"))

    /**
      * Generates a representation of a "bad factor" with indentation and includes the result of showing the associated expression.
      *
      * @param i the current indentation level used for formatting the output.
      * @return a formatted string that describes the "bad factor" along with the indented representation of the associated expression.
      */
    def show(i: Int): String =
      s"bad factor: ${new_line(i)}${x.show(i + 1)}"
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
    * Parses a floating-point number string and converts it to a FloatingPoint object.
    *
    * @return a Parser that processes a floating-point number and returns a Factor in the form of a FloatingPoint instance.
    */
  def number: Parser[Factor] =
    floatingPointNumber ^^ (x => FloatingPoint(x))

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

  /**
    * Creates a new line followed by a specified level of indentation.
    *
    * @param i the number of indentation levels to add after the new line,
    *          where each level corresponds to two spaces
    */
  private def new_line(i: Int) =
    "\n" + "  ".repeat(i)
}

case class ParseException(s: String) extends Exception(s"Parse exception: $s")