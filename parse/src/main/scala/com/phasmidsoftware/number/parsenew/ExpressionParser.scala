/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.algebra.Valuable
import com.phasmidsoftware.number.expression.expr.{Expression, Noop}
import fastparse.Parsed
import scala.util.Try

object ExpressionParser {

  val latexParser: String => Parsed[Expression] = LaTeXParser.parse

  /**
    * Extension method `lazymath` for `StringContext`.
    *
    * This method provides support for creating `Expression` objects by interpolating
    * strings within a `StringContext`. It processes the string parts and interpolated
    * arguments, combining them into a single string, which is then used to construct
    * a new `Expression`.
    *
    * @return an instance of `Expression` derived from the interpolated string.
    */
  extension (inline sc: StringContext)
    /**
      * Parses a LaTeX-like math expression from the provided parts and arguments, 
      * constructing an `Expression` object.
      *
      * @param args Vararg of arguments to be interpolated into the string for parsing.
      * @return An `Expression` object parsed from the constructed string. If parsing fails, it returns a `Noop` with error details.
      *         Throws a `LaTeXParserException` if the input string is not fully parsed.
      */
    inline def puremath(args: Any*): Expression =
      val parts = sc.parts
      val string = (parts.zip(args).flatMap { case (s, a) => Seq(s, a.toString) } ++ parts.drop(args.length)).mkString
      latexParser(string) match {
        case failure: Parsed.Failure =>
          Noop(failure.toString)
        case Parsed.Success(value, index) if index == string.length =>
          value
        case Parsed.Success(_, index) =>
          throw LaTeXParserException(s"ExpressionParser: expected to parse all of $string, but only parsed $index of them")
      }
    inline def lazymath(args: Any*): Expression =
      puremath(args *).simplify
    /**
      * Processes LaTeX-like mathematical expressions using interpolation and evaluates the resulting expression
      * into a `Valuable` instance. This method uses the `lazymath` extension to parse the expression and 
      * subsequently materializes the parsed result into a `Valuable` object.
      *
      * @param args the arguments to be embedded into the LaTeX-like string for parsing and evaluation.
      * @return a `Valuable` object representing the materialized result of the parsed mathematical expression.
      */
    inline def math(args: Any*): Valuable =
      lazymath(args *).materialize
    /**
      * Attempts to process and evaluate a LaTeX-like mathematical expression from the provided arguments, returning
      * an `Option` containing the resulting `Valuable` instance if successful. If the evaluation fails, `None` is returned.
      *
      * @param args vararg of arguments to be interpolated into the mathematical expression for parsing and evaluation.
      * @return an `Option[Valuable]` containing the result of the evaluated mathematical expression if successful, or `None` if
      *         the evaluation fails.
      */
    inline def mathOpt(args: Any*): Option[Valuable] =
      Try(math(args *)).toOption
}

