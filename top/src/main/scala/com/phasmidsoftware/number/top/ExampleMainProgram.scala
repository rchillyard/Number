/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps

@main def exampleMainProgram(): Unit =
  import expr.*

  // Method 1: Start with identity operator
  val expr0 = ∅ + 1 + 2 * 3
  val expr1 = ∅ * 1 * 2 * 3

  // Method 2: String interpolators
  val expr2 = math"1 + 2 * 3" // resulting type is Eager
  val expr3 = lazymath"$expr1∧2 + 3 * $expr1 - 5" // resulting type is a simplified Expression
  val expr4 = puremath"1 + 2 * 3" // resulting type is Expression

  // Method 3: Predefined constants
  val expr5 = one + 2 * 3
  val expr6 = π / 2

  // Method 4: Explicit type annotation
  val expr7: Expression = 1 + 2

  // All of these create Expression trees
  println(expr1.toLatex)
  println(expr2.toLatex)
  println(expr3.toLatex)
  println(expr4.toLatex)
  println(expr5.toLatex)
  println(expr6.toLatex)
  println(expr7.toLatex)
