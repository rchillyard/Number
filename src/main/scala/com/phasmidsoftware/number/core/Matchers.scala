package com.phasmidsoftware.number.core


trait Matchers {

  sealed trait MatchResult[R] {
    def success: Boolean
  }

  case class Match[R](r: R) extends MatchResult[R] {
    def success: Boolean = true
  }

  case class Miss[T, R](t: T) extends MatchResult[R] {
    def success: Boolean = false
  }

  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) => f(t)

  def success[T, R](r: R): Matcher[T, R] = Matcher(_ => Match(r))

  def fail[T, R]: Matcher[T, R] = Matcher(e => Miss(e))

  abstract class Matcher[T, R] extends (T => MatchResult[R]) {

    //    def !!(f: ExpressionFunction): ExpressionMaterializer = ??? //e => f(e.materialize)
    //
    //    def **(f: ExpressionBiFunction, y: Expression): ExpressionMaterializer = ???

    def |(p: Matcher[T, R]): Matcher[T, R] = Matcher(e =>
      this (e) match {
        case v@Match(_) => v
        case Miss(_) => p(e)
      }
    )

    def ~(p: Matcher[T, R]): Matcher[T, R] = Matcher(e =>
      this (e) match {
        case v@Match(_) => v
        case Miss(_) => p(e)
      }
    )
  }

  def value: Matcher[Expression, Number] = {
    case Literal(x) => Match(x)
    case x@ExactNumber(_, _) => Match(x)
    case x@FuzzyNumber(_, _, _) => Match(x)
    case x@(Zero | MinusOne | One) => Match(x.materialize)
    case x => Miss(x)
  }

  def matchValue(x: Number): Matcher[Expression, Number] = e => value(e) match {
    case v@Match(`x`) => v
    case Match(_) => Miss(e)
    case v@Miss(_) => v
  }

}
