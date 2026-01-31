/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation

// The magic: make integer literals become Expressions
object ExpressionPrelude:
  // Pre-defined constants
  val zero: Expression = Literal(WholeNumber(0))
  val one: Expression = Literal(WholeNumber(1))
  val two: Expression = Literal(WholeNumber(2))
  val minusOne: Expression = Literal(WholeNumber(-1))

  // Automatic lifting from Scala primitives
  given Conversion[Int, Expression] = i => Literal(WholeNumber(i))

  given Conversion[Rational, Expression] = i => Literal(RationalNumber(i))

  given Conversion[Double, Expression] = d => Literal(Real(d))

  given Conversion[String, Expression] = s => Expression(s)

  // Make pi, e, etc. available
  val π: Expression = Literal(Angle.pi)
  val e: Expression = Literal(Eager.e)
  val 𝛗: Expression = Root.phi
  val 𝛙: Expression = Root.psi

  // Common functions
  def sqrt(x: Expression): Expression = x ∧ RationalNumber.half

  def sin(x: Expression): Expression = UniFunction(x, Sine)

  def cos(x: Expression): Expression = UniFunction(x, Cosine)

  def exp(x: Expression): Expression = UniFunction(x, Exp)

  def ln(x: Expression): Expression = UniFunction(x, Ln)

// etc.
