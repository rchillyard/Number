/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.util

/**
  * Typeclass for rendering values as LaTeX mathematical expressions.
  *
  * This typeclass provides a way to convert various mathematical types into
  * their LaTeX representation for use in documents, presentations, or web display.
  *
  * @tparam A the type to render as LaTeX
  */
trait LatexRenderer[A] {
  /**
    * Converts a value to its LaTeX representation.
    *
    * @param value the value to render
    * @return a String containing the LaTeX code
    */
  def toLatex(value: A): String
}

/**
  * Companion object for LatexRenderer providing utility methods and syntax extensions.
  */
object LatexRenderer {

  /**
    * Summoner method for accessing typeclass instances.
    *
    * @tparam A the type for which to retrieve the renderer
    * @return the LatexRenderer instance for type A
    */
  def apply[A](implicit renderer: LatexRenderer[A]): LatexRenderer[A] = renderer

  /**
    * Constructor for creating LatexRenderer instances from functions.
    *
    * @param f the function that converts A to LaTeX
    * @tparam A the type to render
    * @return a new LatexRenderer instance
    */
  def instance[A](f: A => String): LatexRenderer[A] =
    (value: A) => f(value)

  /**
    * Syntax extension that adds toLatex method to any type with a LatexRenderer instance.
    *
    * Example usage:
    * {{{
    * import LatexRenderer._
    * val solution = QuadraticSolution(...)
    * val latex = solution.toLatex
    * }}}
    */
  implicit class LatexRendererOps[A](val value: A) extends AnyVal {
    def toLatex(implicit renderer: LatexRenderer[A]): String =
      renderer.toLatex(value)
  }

  /**
    * Helper method to wrap content in LaTeX math mode delimiters.
    *
    * @param content the LaTeX expression
    * @param inline  if true, uses inline math mode ($...$), otherwise display mode ($$...$$)
    * @return the content wrapped in appropriate delimiters
    */
  def mathMode(content: String, inline: Boolean = true): String = {
    if (inline) s"$$$content$$"
    else s"\\[$content\\]"  // Change from s"\\[$$content\\]" to s"\\[$content\\]"
  }

  /**
    * Helper method to create a fraction.
    *
    * @param numerator   the numerator
    * @param denominator the denominator
    * @return LaTeX fraction notation
    */
  def frac(numerator: String, denominator: String): String =
    if (numerator.length <= 2 && denominator.length <= 2)
      s"\\tfrac{$numerator}{$denominator}"
    else
      s"\\frac{$numerator}{$denominator}"

  /**
    * Helper method to create a square root.
    *
    * @param content the radicand
    * @return LaTeX square root notation
    */
  def sqrt(content: String): String =
    s"\\sqrt{$content}"

  /**
    * Helper method to create an nth root.
    *
    * @param n       the root degree
    * @param content the radicand
    * @return LaTeX nth root notation
    */
  def nthRoot(n: Int, content: String): String =
    s"\\sqrt[$n]{$content}"

  /**
    * Helper method to format a sign (+ or -) with appropriate spacing.
    *
    * @param isPositive if true, returns "+", otherwise "-"
    * @return the formatted sign
    */
  def sign(isPositive: Boolean): String =
    if (isPositive) "+" else "-"
}
