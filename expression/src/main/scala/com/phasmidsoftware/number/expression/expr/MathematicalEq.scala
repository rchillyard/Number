package com.phasmidsoftware.number.expression.expr

import cats.Eq

/**
  * Typeclass for mathematical equality that considers expressions equal
  * if they simplify to structurally equivalent forms.
  *
  * This is distinct from structural equality (Eq) which only considers
  * the syntactic structure of expressions.
  */
trait MathematicalEq[A] {
  /**
    * Test if two values are mathematically equal.
    * For expressions, this typically involves simplification before comparison.
    */
  def eqv(x: A, y: A): Boolean

  /**
    * Symbolic operator for mathematical equality.
    */
  def ≡(x: A, y: A): Boolean = eqv(x, y)
}

object MathematicalEq {
  /**
    * Summon a MathematicalEq instance.
    */
  def apply[A](implicit ev: MathematicalEq[A]): MathematicalEq[A] = ev

  /**
    * Create a MathematicalEq instance from a function.
    */
  def instance[A](f: (A, A) => Boolean): MathematicalEq[A] = new MathematicalEq[A] {
    def eqv(x: A, y: A): Boolean = f(x, y)
  }

  /**
    * MathematicalEq for Expression: simplify both sides then use structural Eq.
    */
  implicit def mathematicalEqExpression(implicit eq: Eq[Expression]): MathematicalEq[Expression] =
    instance { (x, y) =>
      eq.eqv(x.simplify, y.simplify)
    }

  /**
    * For types where mathematical equality is the same as structural equality.
    */
  def fromEq[A](implicit eq: Eq[A]): MathematicalEq[A] =
    instance(eq.eqv)
}

/**
  * Syntax extensions for MathematicalEq.
  */
object MathematicalEqSyntax {
  implicit class MathematicalEqOps[A](val x: A) extends AnyVal {
    def ≡(y: A)(implicit meq: MathematicalEq[A]): Boolean = meq.eqv(x, y)

    def mathEq(y: A)(implicit meq: MathematicalEq[A]): Boolean = meq.eqv(x, y)
  }
}