package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class ShuntingYardParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ShuntingYardParser"

  val p = new ShuntingYardParser

  it should "parseInfix 1" in {
    val sy = p.parseInfix("1")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate) yield e.materialize
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe Number.one
  }

  it should "parseInfix 1 + 2" in {
    val sy = p.parseInfix("1 + 2")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate) yield e.materialize
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe Number(3)
  }

  it should "parseInfix ( 1 + 2 ) * 3" in {
    val sy = p.parseInfix("( 1 + 2 ) * 3")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Number] = for (s <- sy.toOption; e <- s.evaluate) yield e.materialize
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe Number(9)
  }

  it should "shuntingYard" in {
    p.parseInfix("( 1 + 3 ) + ( 2 * 3 )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 1 + 3 ) + ( 2 * 3 ) )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
  }

  it should "operator" in {

  }

}
