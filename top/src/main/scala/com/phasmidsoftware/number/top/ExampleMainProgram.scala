/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps

@main def exampleMainProgram(): Unit =
  import expr.*

  // Method 1: Start with identity operator
  val expr0 = ∅ + 1 + 2 * 3 // lazy 7
  val expr1 = ∅ * 1 * 2 * 3 // lazy 6

  // Method 2: String interpolators
  val expr2 = math"1 + 2 * 3" // resulting type is eager 7
  val expr3 = lazymath"$expr1∧2 + 3 * $expr1 - 5" // resulting type is a simplified Expression with (lazy) value 49
  val expr4 = puremath"1 + 2 * 3" // resulting type is lazy 7

  // Method 3: Predefined constants
  val expr5 = one + 2 * 3 // lazy 7
  val expr6 = π / 2 // lazy ½𝛑
  val expr7 = sin(expr6) // lazy 1

  // Method 4: Explicit type annotation
  val expr8: Expression = 1 + 2 // lazy 3

  // All of these create Expression trees
  println(expr0.toLatex)
  println(expr1.toLatex)
  println(expr2.toLatex)
  println(expr3.toLatex)
  println(expr4.toLatex)
  println(expr5.toLatex)
  println(expr6.toLatex)
  println(expr7.toLatex)
  println(expr8.toLatex)
