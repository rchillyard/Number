/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parsenew

import com.phasmidsoftware.number.expression.expr.{Expression, Noop}
import fastparse.Parsed

object ExpressionParser {

  val latexParser: String => Parsed[Expression] = LaTeXParser.parse

  /**
    * Extension method `math` for `StringContext`.
    *
    * This method provides support for creating `Expression` objects by interpolating
    * strings within a `StringContext`. It processes the string parts and interpolated
    * arguments, combining them into a single string, which is then used to construct
    * a new `Expression`.
    *
    * @param args the arguments to interpolate into the string context.
    * @return an instance of `Expression` derived from the interpolated string.
    */
  extension (inline sc: StringContext)
    inline def math(args: Any*): Expression =
      val parts = sc.parts
      val string = (parts.zip(args).flatMap { case (s, a) => Seq(s, a.toString) } ++ parts.drop(args.length)).mkString
      latexParser(string) match {
        case failure: Parsed.Failure =>
          Noop
        case Parsed.Success(value, index) if index == string.length =>
          value
        case Parsed.Success(_, index) =>
          throw LaTeXParserException(s"ExpressionParser: expected to parse all of $string, but only parsed $index of them")
      }
}

