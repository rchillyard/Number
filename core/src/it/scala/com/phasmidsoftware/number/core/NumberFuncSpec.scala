package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NumberFuncSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "sqrt"
  it should "work for the Basel approximation of pi" in {
    def inverseSquare(x: Int): Rational = Rational.one / (x * x)

    val terms: LazyList[Rational] = LazyList.from(1) map inverseSquare
    val significantTerms = terms takeWhile (x => x.toDouble > 1E-6) to List
    val insignificantTerms = terms map (x => x.toDouble) dropWhile (x => x > 1E-6) takeWhile (x => x > 1E-8) to List
    val basel: Rational = significantTerms.sum * 6
    val error: Double = insignificantTerms.sum * 6
    val piSquared: Number = Number.create(Value.fromRational(basel), AbsoluteFuzz(error, Box))
    val pi = piSquared.sqrt.normalize
    pi.toString shouldBe "3.14063[86]"
  }
}
