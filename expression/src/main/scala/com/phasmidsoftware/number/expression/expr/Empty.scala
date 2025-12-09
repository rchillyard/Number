/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.WholeNumber

/**
  * Represents an object with no functionality or state.
  *
  * This is only defined here so that we no longer have a file without a corresponding class or object.
  */
object Empty

/**
  * The `∅` object represents the identity element in mathematical operations on expressions.
  * It serves as a symbolic representation of the "empty" or neutral element in addition and multiplication operations.
  *
  * This object supports operations such as addition, subtraction, multiplication, and division,
  * which adhere to mathematical identity properties or represent specific transformations when combined with expressions.
  *
  * Functions:
  *
  * - Addition (`+`):
  * Performs addition with an expression or integer. For expressions, it returns the operand itself, acting as the addition identity.
  * For integers, it creates a `Literal` representation of the integer and returns that.
  *
  * - Multiplication (`*`):
  * Performs multiplication with an expression or integer. For expressions, it returns the operand itself, acting as the multiplication identity.
  * For integers, it creates a `Literal` representation of the integer and returns that.
  *
  * - Division (`/`):
  * Represents the division operation. For expressions or integers, it creates a transformed expression wrapped in the `Reciprocal` operation.
  *
  * - Subtraction (`-`):
  * Represents subtraction or negation. For expressions or integers, it creates a transformed expression wrapped in the `Negate` operation.
  */
object ∅ {
  // Addition identity
  def +(operand: Expression): Expression = operand

  def +(operand: Int): Expression = Literal(WholeNumber(operand))

  // Multiplication identity
  def *(operand: Expression): Expression = operand

  def *(operand: Int): Expression = Literal(WholeNumber(operand))

  // Division
  def /(operand: Expression): Expression = UniFunction(operand, Reciprocal)

  def /(operand: Int): Expression = UniFunction(Literal(WholeNumber(operand)), Reciprocal)

  // Subtraction (negation)
  def -(operand: Expression): Expression = UniFunction(operand, Negate)

  def -(operand: Int): Expression = UniFunction(Literal(WholeNumber(operand)), Negate)
}