/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.parse

import com.phasmidsoftware.number.core.mill.CoreMill
import com.phasmidsoftware.number.core.numerical.Number
import com.phasmidsoftware.number.core.parse.ShuntingYardParser
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class ShuntingYardParserSpec extends AnyFlatSpec with should.Matchers {

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "ShuntingYardParser"
  private val p = ShuntingYardParser
//  it should "parseInfix 1" in {
//    val sy = p.parseInfix("1")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number.one
//  }
//  it should "parseInfix 1 + 2" in {
//    val sy = p.parseInfix("1 + 2")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number(3)
//  }
//  it should "parseInfix ( 1 + 2 ) * 3" in {
//    val sy = p.parseInfix("( 1 + 2 ) * 3")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number(9)
//  }
  // From https://www.hpmuseum.org/rpn.htm
//  it should "parseInfix ( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ∧ 9" in {
//    val sy = p.parseInfix("( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ∧ 9")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number("60716.992766464000")
//  }
//  it should "parseInfix ( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ∧ 2 ) ∧ 3 )" in {
//    val sy = p.parseInfix("( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ∧ 2 ) ∧ 3 )")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number(64)
//  }
////  it should "parseInfix 3 + 4 × 2 / ( 1 − 5 ) ∧ 2 ∧ 3" in {
////    val sy = p.parseInfix("3 + 4 × 2 / ( 1 - 5 ) ∧ 2 ∧ 3")
////    sy should matchPattern { case Success(_) => }
////    // 3 4 2 × 1 5 − 2 3 ∧ ∧ ÷ +
////    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
////    xo should matchPattern { case Some(_) => }
////    xo.get shouldEqual Number(3.0001220703125)
////  }
//  it should "parseInfix ( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )" in {
//    val sy = p.parseInfix("( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )")
//    sy should matchPattern { case Success(_) => }
//    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate; n <- convertMillExpressionToExpression(e).asNumber) yield n
//    xo should matchPattern { case Some(_) => }
//    xo.get shouldBe Number(101)
//  }
  it should "parse Infix and evaluate:  9" in {
    val value: Try[CoreMill] = p.parseInfix("3 ∧ 2")
    value should matchPattern { case Success(_) => }
    value map (_.evaluate shouldBe Some(9))
  }
  it should "parse Infix and evaluate:  0.5" in {
    val value: Try[CoreMill] = p.parseInfix("2 ∧ -1")
    value should matchPattern { case Success(_) => }
    value map (_.evaluate shouldBe Some(0.5))
  }
  it should "shuntingYard" in {
    p.parseInfix("( 1 + 3 ) + ( 2 * 3 )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 1 + 3 ) + ( 2 * 3 ) )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
  }
  // Test for (fixed) Issue #42
  it should "shuntingYard w/o spaces" in {
    p.parseInfix("(1+3)+(2*3)") should matchPattern { case Success(_) => }
    p.parseInfix("((1+3)+(2*3))") should matchPattern { case Success(_) => }
    p.parseInfix("((0.5+3)+(2*(0.5+3)))") should matchPattern { case Success(_) => }
  }
  it should "operator" in {

  }
}
