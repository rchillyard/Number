/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.expression.parse.TokenType.{DyadicOperator, MonadicOperator, Number}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class ExpressionParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExpressionParser"

  val p = ExpressionParser()

  it should "apply number expressions" in {
    p.apply("1") shouldBe Success(Expression(Seq(ExpressionToken("1", Number))))
    p.apply("1+2") shouldBe Success(ExpressionTokens(Seq(ExpressionToken("1", Number), ExpressionToken("+", DyadicOperator), ExpressionToken("2", Number))))
    p.apply("6*7") shouldBe Success(ExpressionTokens(Seq(ExpressionToken("6", Number), ExpressionToken("*", DyadicOperator), ExpressionToken("7", Number))))
    p.apply("6*(3+4)") shouldBe Success(ExpressionTokens(Seq(ExpressionToken("6", Number), ExpressionToken("*", DyadicOperator), ExpressionToken("3", Number), ExpressionToken("+", DyadicOperator), ExpressionToken("4", Number))))
    p.apply("42/6") shouldBe Success(ExpressionTokens(Seq(ExpressionToken("42", Number), ExpressionToken("*", DyadicOperator), ExpressionToken("/", MonadicOperator), ExpressionToken("6", Number))))
  }
  it should "apply pi" in {
    p.apply("pi") shouldBe Success(Expression(Seq(ExpressionToken("Pi", Number))))
    p.apply("π") shouldBe Success(Expression(Seq(ExpressionToken("Pi", Number))))
    p.apply("𝛑") shouldBe Success(Expression(Seq(ExpressionToken("Pi", Number))))
    p.apply("""\uD835\uDED1""") shouldBe Success(Expression(Seq(ExpressionToken("Pi", Number))))
  }
  it should "apply e" in {
    p.apply("e") shouldBe Success(Expression(Seq(ExpressionToken("Euler", Number))))
    p.apply("""𝜀""") shouldBe Success(Expression(Seq(ExpressionToken("Euler", Number))))
  }
}
