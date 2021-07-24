package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.{Expression, GeneralNumber}
import com.phasmidsoftware.number.mill.Mill
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class ShuntingYardParserSpec extends AnyFlatSpec with should.Matchers {


  implicit object NumberEquality extends Equality[GeneralNumber] {
    def areEqual(a: GeneralNumber, b: Any): Boolean = b match {
      case n: GeneralNumber => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "ShuntingYardParser"

  val p = new ShuntingYardParser

  it should "parseInfix 1" in {
    val sy = p.parseInfix("1")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber.one
  }

  it should "parseInfix 1 + 2" in {
    val sy = p.parseInfix("1 + 2")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber(3)
  }

  it should "parseInfix ( 1 + 2 ) * 3" in {
    val sy = p.parseInfix("( 1 + 2 ) * 3")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber(9)
  }
  // From https://www.hpmuseum.org/rpn.htm
  it should "parseInfix ( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ^ 9" in {
    val sy = p.parseInfix("( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ^ 9")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber("60716.992766464000")
  }
  it should "parseInfix ( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ^ 2 ) ^ 3 )" in {
    val sy = p.parseInfix("( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ^ 2 ) ^ 3 )")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber(64)
  }
  it should "parseInfix 3 + 4 × 2 / ( 1 − 5 ) ^ 2 ^ 3" in {
    val sy = p.parseInfix("3 + 4 × 2 / ( 1 - 5 ) ^ 2 ^ 3")
    sy should matchPattern { case Success(_) => }
    // 3 4 2 × 1 5 − 2 3 ^ ^ ÷ +
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldEqual GeneralNumber(3.0001220703125)
  }

  it should "parseInfix ( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )" in {
    val sy = p.parseInfix("( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )")
    sy should matchPattern { case Success(_) => }
    val xo: Option[GeneralNumber] = for (s <- sy.toOption; e <- s.evaluate; n <- e.materialize.asNumber) yield n
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe GeneralNumber(101)
  }

  it should "parse Infix and evaluate:  9" in {
    val value: Try[Mill] = p.parseInfix("3 ^ 2")
    value should matchPattern { case Success(_) => }
    value map (_.evaluate shouldBe 9)
  }
  it should "parse Infix and evaluate:  0.5" in {
    val value: Try[Mill] = p.parseInfix("2 ^ -1")
    value should matchPattern { case Success(_) => }
    value map (_.evaluate shouldBe 0.5)
  }

  it should "parse Infix and evaluate: sqrt(3)" in {
    val value: Option[Mill] = p.parseInfix("3 ^ ( 2 ^ -1 )").toOption
    value should matchPattern { case Some(_) => }
    val z: Option[Expression] = value flatMap (_.evaluate)
    val q = z map (_.materialize)
    // TODO test the result.
  }


  it should "shuntingYard" in {
    p.parseInfix("( 1 + 3 ) + ( 2 * 3 )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 1 + 3 ) + ( 2 * 3 ) )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
  }

  it should "operator" in {

  }

}
