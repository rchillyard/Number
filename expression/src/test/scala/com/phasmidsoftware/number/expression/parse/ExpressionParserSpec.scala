/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.expression.parse.TokenType.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class ExpressionParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExpressionParser"

  val p = ExpressionParser()

  it should "apply" in {
    p.apply("1") shouldBe Success(Expression(Seq(ExpressionToken("1", Number))))
  }
}
