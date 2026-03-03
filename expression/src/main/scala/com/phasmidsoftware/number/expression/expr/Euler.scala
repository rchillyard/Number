/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.expression.expr.Expression.em

/**
  * Represents the canonical lazy complex form r*e^(i*theta), per Euler's formula.
  *
  * This is the canonical lazy representation for complex exponential and trigonometric
  * expressions in the Number library. It materialises to `ComplexPolar` on evaluation.
  *
  * Special cases:
  *  - `Euler(Expression.one, theta)` is unit modulus: e^(i*theta) = cos(theta) + i*sin(theta)
  *  - `Euler(r, Expression.zero)` is a real number r (materialisation simplifies to r)
  *  - `Euler(Expression.one, Pi)` simplifies to -1 (Euler's identity)
  *
  * @param r     the modulus expression (non-negative real)
  * @param theta the argument (angle) expression, in radians
  */
case class Euler(r: Expression, theta: Expression) extends CompositeExpression {

  // TODO implement full Euler support (Version 1.8)

  def maybeFactor(context: Context): Option[Factor] =
    evaluate(context) flatMap (v => v.maybeFactor(context))

  lazy val isExact: Boolean = r.isExact && theta.isExact

  lazy val renderAsExpression: String =
    r.render + "*e^(i*" + theta.render + ")"

  lazy val terms: Seq[Expression] = Seq(r, theta)

  lazy val depth: Int = 1 + math.max(r.depth, theta.depth)

  /** TODO: materialise to ComplexPolar(r, theta) */
  def evaluate(context: Context): Option[Eager] = None

  /** TODO: approximate via ComplexPolar */
  def approximation(force: Boolean): Option[eager.Real] = None

  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: operandsMatcher") {
      case x => em.Miss("Euler: operandsMatcher: not yet implemented", x)
    }

  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: identitiesMatcher") {
      case x => em.Miss("Euler: identitiesMatcher: not yet implemented", x)
    }

  lazy val structuralMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: structuralMatcher") {
      case x => em.Miss("Euler: structuralMatcher: not yet implemented", x)
    }
}

object Euler {
  // TODO add unapply extractors and simplification helpers (Version 1.8)
}
