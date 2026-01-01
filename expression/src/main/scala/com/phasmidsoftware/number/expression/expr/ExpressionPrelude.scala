/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.*

// The magic: make integer literals become Expressions
object ExpressionPrelude:
  // Pre-defined constants
  val zero: Expression = Literal(WholeNumber(0))
  val one: Expression = Literal(WholeNumber(1))
  val two: Expression = Literal(WholeNumber(2))
  // ... could generate programmatically for -100 to 100

  // Automatic lifting from Scala primitives
  given Conversion[Int, Expression] = i => Literal(WholeNumber(i))

  given Conversion[Double, Expression] = d => Literal(Real(d))
//  given Conversion[String, Expression] = s => Variable(s) // or parse as LaTeX

  // Make pi, e, etc. available
  val π: Expression = Literal(Angle.pi)
  val e: Expression = Literal(Eager.e)

  // Common functions
  def sqrt(x: Expression): Expression = x ∧ RationalNumber.half

  def sin(x: Expression): Expression = UniFunction(x, Sine)

  def cos(x: Expression): Expression = UniFunction(x, Cosine)
// etc.
