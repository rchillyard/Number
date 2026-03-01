/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.algebra.core.Renderable
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.expression.expr.{Expression, Noop}

import scala.util.Try

/**
  * The `ExpressionParser` object provides utilities for parsing and processing LaTeX-like
  * mathematical expressions, leveraging string interpolation. These utilities enable creation,
  * simplification, and evaluation of mathematical expressions represented as `Expression`
  * objects.
  *
  * TODO instead of throwing exceptions we should return a `ParseError` object.
  */
object ExpressionParser {

  /**
    * Parses a LaTeX-like mathematical expression from the provided string and returns
    * an `Expression` object.
    *
    * @param string the input string containing the LaTeX-like expression to be parsed.
    * @return an `Expression` object parsed from the input string, or a `Noop` on failure.
    *         Throws a `LaTeXParserException` if the input is not fully consumed.
    */
  def parse(string: String): Expression =
    LaTeXParser.parse(string) match {
      case LaTeXParser.Failure(msg, _) =>
        Noop(msg)
      case LaTeXParser.Error(msg, _) =>
        Noop(msg)
      case LaTeXParser.Success(value, next) if next.atEnd =>
        value
      case LaTeXParser.Success(_, next) =>
        throw LaTeXParserException(
          s"ExpressionParser: expected to parse all of $string, but stopped at offset ${next.offset}"
        )
    }

  extension (inline sc: StringContext)

    /**
      * Parses a LaTeX-like math expression from the provided parts and arguments,
      * constructing an `Expression` object.
      */
    inline def puremath(args: Any*): Expression =
      val parts = sc.parts
      val string = (parts.zip(args).flatMap {
        case (s, a: Renderable) => Seq(s, a.render)
        case (s, a) => Seq(s, a.toString)
      } ++ parts.drop(args.length)).mkString
      parse(string)

    inline def lazymath(args: Any*): Expression =
      puremath(args *).simplify

    /**
      * Parses and materializes a LaTeX-like expression into an `Eager` value.
      */
    inline def math(args: Any*): Eager =
      lazymath(args *).materialize

    /**
      * Like `math`, but returns `None` if evaluation fails.
      */
    inline def mathOpt(args: Any*): Option[Eager] =
      Try(math(args *)).toOption

    /**
      * Like `math`, but applies fuzzy evaluation.
      */
    inline def fuzzymath(args: Any*): Eager =
      math(args *).fuzzy
}