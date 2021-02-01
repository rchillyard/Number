package com.phasmidsoftware.number.core


trait ExpressionMaterializers {

  sealed trait MaterializerResult[R] {
    def success: Boolean
  }

  case class Evaluation[R](x: R) extends MaterializerResult[R] {
    def success: Boolean = true
  }

  case class NonMaterialized[R](e: Expression) extends MaterializerResult[R] {
    def success: Boolean = false
  }

  def ExpressionMaterializer[R](f: Expression => MaterializerResult[R]): ExpressionMaterializer[R] = (e: Expression) => f(e)

  abstract class ExpressionMaterializer[R] extends (Expression => MaterializerResult[R]) {

//    def !!(f: ExpressionFunction): ExpressionMaterializer = ??? //e => f(e.materialize)
//
//    def **(f: ExpressionBiFunction, y: Expression): ExpressionMaterializer = ???

    def |(p: ExpressionMaterializer[R]): ExpressionMaterializer[R] = ExpressionMaterializer(e =>
      this (e) match {
        case v@Evaluation(_) => v
        case NonMaterialized(_) => p(e)
      }
    )

    def ~(p: ExpressionMaterializer[R]): ExpressionMaterializer[R] = ExpressionMaterializer(e =>
      this (e) match {
        case v@Evaluation(_) => v
        case NonMaterialized(_) => p(e)
      }
    )
  }

  def value: ExpressionMaterializer[Number] = {
    case Literal(x) => Evaluation(x)
    case x@ExactNumber(_, _) => Evaluation(x)
    case x@FuzzyNumber(_, _, _) => Evaluation(x)
    case x@(Zero | MinusOne | One) => Evaluation(x.materialize)
    case x => NonMaterialized(x)
  }

  def matchValue(x: Number): ExpressionMaterializer[Number] = e => value(e) match {
    case v@Evaluation(`x`) => v
    case Evaluation(_) => NonMaterialized(e)
    case v@NonMaterialized(_) => v
  }

}
