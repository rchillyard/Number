/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps

@main def exampleMainProgram(): Unit =
  import expr._

  val result = ∅ + 1 + 2 * 3
  println(result)
  println(result.render)
  println(result.getClass)
  println(result.toLatex)
//
//@main def exampleMainProgram(): Unit =
//  import expr._
//
////  println(∅.getClass)  // What is ∅ actually?
//  val expr0 = ∅ + 1
//  println(expr0.getClass)  // What does ∅ + 1 give us?
//
//  // Method 1: Empty operator
//  val expr1 = ∅ + 1 + 2
//
//  // Method 2: String interpolators
//  val expr2 = math"1 + 2 * 3"
//  val expr3 = lazymath"x^2 + 3x - 5"
//  val expr4 = mathOpt"maybe an expression"  // returns Option[Expression]
//
//  // Method 3: Predefined constants
//  val expr5 = one + 2 * 3
//  val expr6 = π / 2
//
//  // Method 4: Variables
////  val expr7 = Variable("x") + 1
//
//  // All of these create Expression trees
//  println(expr1.toLatex)
//  println(expr2.toLatex)
//  println(expr3.toLatex)