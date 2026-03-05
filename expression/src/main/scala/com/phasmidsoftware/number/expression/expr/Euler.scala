/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.core.Valuable.valuableToMaybeField
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.{Angle, Complex, Eager, WholeNumber}
import com.phasmidsoftware.number.core.inner.{Factor, Radian}
import com.phasmidsoftware.number.core.numerical.ComplexPolar
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

  override lazy val toString: String = s"Euler(${r.show}, ${θ.show})"

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
      rField <- valuableToMaybeField(rVal)
      rNum <- rField.asNumber
      tField <- valuableToMaybeField(tVal)
      tNum <- tField.asNumber
      tRad = ensureRadian(tNum)
    } yield
      Complex(ComplexPolar(rNum, tRad))()

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
      case e: Euler if !e.terms.exists(_.isInstanceOf[Euler]) =>
        componentsSimplifier(e.terms, { xs => val Seq(newR, newTheta) = xs; Euler(newR, newTheta) })
      case e: Euler =>
        em.Miss("Euler: operandsMatcher: contains Euler operand", e)
    }

  /**
    * Materialization rules: special values of r or theta that reduce to simpler forms.
    */
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Euler:identitiesMatcher") {
      // Literal(Angle) r = 1 special cases
      case Euler(One, IsEager(a: Angle)) if a.normalize.isZero =>
        em.Match(One)
      case Euler(One, IsEager(a: Angle)) if a.normalize == Angle.pi =>
        em.Match(MinusOne)
      case Euler(One, IsEager(a: Angle)) if a.normalize == Angle.piBy2 =>
        em.Match(I)
      case Euler(One, IsEager(a: Angle)) if a.normalize == Angle.negPiBy2 =>
        em.Match.of(UniFunction(I, Negate))
      // Literal(Angle) general r cases
      case Euler(r, IsEager(a: Angle)) if a.normalize.isZero =>
        em.Match(r)
      case Euler(r, IsEager(a: Angle)) if a.normalize == Angle.pi =>
        em.Match.of(UniFunction(r, Negate))
      case Euler(r, IsEager(a: Angle)) if a.normalize == Angle.piBy2 =>
        em.Match.of(BiFunction(r, I, Product))
      case Euler(r, IsEager(a: Angle)) if a.normalize == Angle.negPiBy2 =>
        em.Match.of(UniFunction(BiFunction(r, I, Product), Negate))
      // Zero modulus => 0 regardless of angle
      case Euler(Zero, _) =>
        em.Match(Zero)
      // General r, structural angle cases
      case Euler(r, Zero) =>
        em.Match(r)
      case Euler(r, Pi) =>
        em.Match.of(UniFunction(r, Negate))
      case Euler(r, Euler.HalfPi()) =>
        em.Match.of(BiFunction(r, I, Product))
      case Euler(r, Euler.MinusHalfPi()) =>
        em.Match.of(UniFunction(BiFunction(r, I, Product), Negate))
      // Structural angle = 2π (full rotation = 0)
      case Euler(r, BiFunction(Pi, Two, Product)) =>
        em.Match(r)
      case Euler(r, BiFunction(Two, Pi, Product)) =>
        em.Match(r)
      // Structural angle = -2π
      case Euler(r, UniFunction(BiFunction(Pi, Two, Product), Negate)) =>
        em.Match(r)
      case Euler(r, UniFunction(BiFunction(Two, Pi, Product), Negate)) =>
        em.Match(r)
      // Structural angle = 3π (= π after normalization)
      case Euler(r, BiFunction(Pi, IsEager(WholeNumber.three), Product)) =>
        em.Match.of(UniFunction(r, Negate))

      // TODO option 2: evaluate angle to Literal(Angle) in operandsMatcher for uniform handling

      case Euler(r, BiFunction(IsEager(WholeNumber.three), Pi, Product)) =>
        em.Match.of(UniFunction(r, Negate))
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
    em.Matcher("Euler:structuralMatcher") {
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
    * Ensures the given core.numerical.Number carries the Radian factor
    * required by ComplexPolar. If already in Radian, returned unchanged;
    * otherwise scaled.
    */
  private def ensureRadian(n: com.phasmidsoftware.number.core.numerical.Number): com.phasmidsoftware.number.core.numerical.Number =
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

/**
  * The `IsEuler` object provides functionality to extract an instance
  * of `Euler` from a given `Expression` if the expression conforms to a
  * known form of Euler's representation. It enables pattern matching
  * for expressions representing Euler's equation or its variations.
  */
object IsEuler {
  /**
    * Extracts an instance of Euler from a given Expression, if the expression matches
    * a known Euler form.
    *
    * @param e the expression to be evaluated for extraction
    * @return an Option containing the extracted Euler instance if the expression matches;
    *         None otherwise
    */
  def unapply(e: Expression): Option[Euler] = e match {
    case MinusOne =>
      Some(Euler(One, Pi))
    case I =>
      Some(Euler(One, BiFunction(Pi, Half, Product)))
    case UniFunction(IsEuler(eu), Negate) =>
      Some(Euler(eu.r, UniFunction(eu.θ, Negate)))
    case _ =>
      None
  }
}