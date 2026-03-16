/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

/**
  * Concise pattern-match extractors for common `UniFunction` expression shapes.
  *
  * These allow simplification rules (e.g. Euler recognition) to be written
  * in readable, mathematical style:
  *
  * {{{
  *   case BiFunction(Cos(θ), BiFunction(I, Sin(θ2), Product), Sum) => ...
  *   case Exp(BiFunction(I, x, Product))                           => ...
  *   case Exp(BiFunction(a, BiFunction(I, b, Product), Sum))       => ...
  * }}}
  *
  * Each extractor matches `UniFunction(arg, FunctionCaseObject)` and returns
  * the inner argument. No changes to `core` are required.
  */

/** Matches `sin(θ)` → `UniFunction(θ, Sine)`, returns `θ`. */
object Sin {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(θ, Sine) => Some(θ)
    case _ => None
  }
}

/** Matches `cos(θ)` → `UniFunction(θ, Cosine)`, returns `θ`. */
object Cos {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(θ, Cosine) => Some(θ)
    case _ => None
  }
}

/**
  * Matches `exp(x)` → `UniFunction(x, Exp)`, returns `x`.
  *
  * NOTE: if the name `Exp` clashes with the `ExpressionFunction` case object
  * of the same name inside the match body, rename this extractor to `ExpX`.
  */
object ExpX {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(x, Exp) => Some(x)
    case _ => None
  }
}

/** Matches `sinh(x)` → `UniFunction(x, Sinh)`, returns `x`. */
object SinhX {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(x, Sinh) => Some(x)
    case _ => None
  }
}

/** Matches `cosh(x)` → `UniFunction(x, Cosh)`, returns `x`. */
object CoshX {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(x, Cosh) => Some(x)
    case _ => None
  }
}