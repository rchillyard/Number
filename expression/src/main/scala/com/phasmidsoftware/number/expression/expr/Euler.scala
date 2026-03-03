/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.{Complex, Eager}
import com.phasmidsoftware.number.core.inner.{Factor, Radian}
import com.phasmidsoftware.number.core.numerical.{ComplexPolar, Number}
import com.phasmidsoftware.number.expression.expr.Expression.em
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier

/**
  * Represents the canonical lazy complex form r*e^(i*theta), per Euler's formula.
  *
  * This is the canonical lazy representation for complex exponential and trigonometric
  * expressions in the Number library. It materialises to `ComplexPolar` on evaluation.
  *
  * Conversion rules (applied by the simplification pipeline, not at construction):
  *  - `cos(theta) + i*sin(theta)`  =>  `Euler(one, theta)`     (Euler recognition)
  *  - `exp(i*theta)`               =>  `Euler(one, theta)`     (Euler recognition)
  *  - `exp(a + i*b)`               =>  `Euler(exp(a), b)`      (Euler recognition)
  *
  * Special materialisation cases:
  *  - `Euler(r, 0)`     =>  r            (real result)
  *  - `Euler(r, Pi)`    =>  Negate(r)    (negative real)
  *  - `Euler(1, 0)`     =>  1
  *  - `Euler(1, Pi)`    =>  -1           (Euler's identity)
  *  - `Euler(1, Pi/2)`  =>  i
  *  - `Euler(0, theta)` =>  0            (zero modulus)
  *
  * Arithmetic rules (applied by the simplification pipeline):
  *  - `Euler(r1,t1) * Euler(r2,t2)` =>  `Euler(r1*r2, t1+t2)`  (de Moivre multiplication)
  *  - `Euler(r1,t1) / Euler(r2,t2)` =>  `Euler(r1/r2, t1-t2)`  (de Moivre division)
  *  - `Euler(r,t) ^ n`              =>  `Euler(r^n, n*t)`       (de Moivre's theorem)
  *  - `Euler(r,t) + Euler(r,-t)`    =>  `2*r*cos(t)`           (conjugate sum => real)
  *  - `Euler(r,t) - Euler(r,-t)`    =>  `2*i*r*sin(t)`         (conjugate diff => imaginary)
  *
  * @param r the modulus expression (non-negative real)
  * @param θ the argument (angle) expression, in radians
  */
case class Euler(r: Expression, θ: Expression) extends CompositeExpression {

  def maybeFactor(context: Context): Option[Factor] =
    evaluate(context) flatMap (v => v.maybeFactor(context))

  lazy val isExact: Boolean = r.isExact && θ.isExact

  /**
    * Renders as r*e^(i*theta) using the render representations of r and theta.
    */
  lazy val renderAsExpression: String =
    r.render + "*e^(i*" + θ.render + ")"

  lazy val terms: Seq[Expression] = Seq(r, θ)

  lazy val depth: Int = 1 + math.max(r.depth, θ.depth)

  /**
    * Materialises this Euler expression to an algebra.Complex wrapping a ComplexPolar.
    *
    * Evaluates r and theta independently, then ensures theta carries the Radian
    * factor required by ComplexPolar's constructor. Returns None if either
    * sub-expression cannot be evaluated.
    */
  def evaluate(context: Context): Option[Eager] =
    for {
      rVal <- r.evaluate(context)
      tVal <- θ.evaluate(context)
      rNum <- asNumber(rVal)
      tNum <- asNumber(tVal)
      tRad = ensureRadian(tNum)
    } yield Complex(ComplexPolar(rNum, tRad))()

  /**
    * Approximates this Euler expression via ComplexPolar.
    * CONSIDER implement via ComplexPolar approximation
    */
  def approximation(force: Boolean): Option[eager.Real] = None

  /**
    * Simplifies r and theta independently, rebuilding Euler with any simplified operands.
    */
  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: operandsMatcher") {
      case e: Euler =>
        componentsSimplifier(e.terms, { xs => val Seq(newR, newTheta) = xs; Euler(newR, newTheta) })
    }

  /**
    * Materialisation rules: special values of r or theta that reduce to simpler forms.
    */
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: identitiesMatcher") {
      // Zero modulus => 0 regardless of angle
      case Euler(Zero, _) =>
        em.Match(Zero)
      // Angle = 0 => result is just r (real)
      case Euler(r, Zero) =>
        em.Match(r)
      // Angle = Pi => result is -r (negative real)
      case Euler(r, Pi) =>
        em.Match(-r)
      // Angle = Pi/2 => result is i*r (pure imaginary)
      case Euler(r, Euler.HalfPi()) =>
        em.Match(r * I)
      // Angle = -Pi/2 => result is -i*r
      case Euler(r, Euler.MinusHalfPi()) =>
        em.Match(-(r * I))
      // r = 1 special cases (Euler's identity and friends)
      case Euler(One, Pi) =>
        em.Match(MinusOne)
      case Euler(One, Zero) =>
        em.Match(One)
      case Euler(One, Euler.HalfPi()) =>
        em.Match(I)
      case Euler(One, Euler.MinusHalfPi()) =>
        em.Match(-I)
      case x =>
        em.Miss("Euler: identitiesMatcher: no special value", x)
    }

  /**
    * Structural rules: de Moivre arithmetic combining two Euler expressions.
    *
    * Note: there is no Difference operator in ExpressionBiFunction. Division is
    * expressed as Product with Reciprocal applied to the denominator, and subtraction
    * is expressed as Sum with Negate applied to the subtrahend.
    */
  lazy val structuralMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler: structuralMatcher") {
      // Multiplication: add angles, multiply moduli
      case BiFunction(Euler(r1, t1), Euler(r2, t2), Product) =>
        em.Match(Euler(r1 * r2, t1 + t2))
      // Division: subtract angles, divide moduli
      // (expressed as Euler * Reciprocal(Euler) since there is no Difference operator)
      case BiFunction(Euler(r1, t1), UniFunction(Euler(r2, t2), Reciprocal), Product) =>
        em.Match(Euler(r1 * UniFunction(r2, Reciprocal), t1 - t2))
      // Power: de Moivre's theorem
      case BiFunction(Euler(r, t), n, Power) =>
        em.Match(Euler(r ∧ n, t * n))
      // Conjugate sum => 2*r*cos(theta) (real)
      case BiFunction(Euler(r1, t), Euler(r2, UniFunction(t2, Negate)), Sum) if r1 == r2 && t == t2 =>
        em.Match(Two * r1 * UniFunction(t, Cosine))
      // Conjugate difference (sum with negated second term) => 2*i*r*sin(theta) (pure imaginary)
      case BiFunction(Euler(r1, t), UniFunction(Euler(r2, UniFunction(t2, Negate)), Negate), Sum) if r1 == r2 && t == t2 =>
        em.Match(Two * I * r1 * UniFunction(t, Sine))
      case x =>
        em.Miss("Euler: structuralMatcher: no structural simplification", x)
    }

  // ---------------------------------------------------------------------------
  // Private helpers
  // ---------------------------------------------------------------------------

  /**
    * Extracts a Number from an Eager value.
    * Euler materialisation requires both r and theta to be plain Numbers.
    */
  private def asNumber(e: Eager): Option[Number] = e match {
    case n: Number => Some(n)
    case _ => None
  }

  /**
    * Ensures the given Number carries the Radian factor required by ComplexPolar.
    * If it is already in Radian, it is returned unchanged; otherwise it is scaled.
    */
  private def ensureRadian(n: Number): Number =
    if (n.factor == Radian) n else n.scale(Radian)
}

/**
  * Companion object for Euler.
  */
object Euler {

  /**
    * Convenience constructor for the unit-modulus case e^(i*theta).
    * Equivalent to Euler(One, theta).
    */
  def apply(θ: Expression): Euler = Euler(One, θ)

  // ---------------------------------------------------------------------------
  // Extractors used by identitiesMatcher
  // ---------------------------------------------------------------------------

  /**
    * Extractor matching Pi/2 in expression form.
    */
  object HalfPi {
    def unapply(e: Expression): Boolean = e match {
      case BiFunction(Pi, Half, Product) | BiFunction(Half, Pi, Product) => true
      case _ => false
    }
  }

  /**
    * Extractor matching -Pi/2 in expression form.
    */
  object MinusHalfPi {
    def unapply(e: Expression): Boolean = e match {
      case UniFunction(BiFunction(Pi, Half, Product), Negate) => true
      case UniFunction(BiFunction(Half, Pi, Product), Negate) => true
      case BiFunction(Pi, UniFunction(Half, Negate), Product) => true
      case _ => false
    }
  }
}