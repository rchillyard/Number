/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.parse

import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Number}
import com.phasmidsoftware.number.core.parse.ComplexParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util._

class ComplexParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ComplexParser"

  it should "doParse" in {

    val parser = new ComplexParser
    parser.doParse("1i0") should matchPattern { case Success(ComplexCartesian(Number.one, Number.zero)) => }
    parser.doParse("1i1") should matchPattern { case Success(ComplexCartesian(Number.one, Number.one)) => }
    parser.doParse("0i1") should matchPattern { case Success(ComplexCartesian(Number.zero, Number.one)) => }
    parser.doParse("1ipi") should matchPattern { case Success(ComplexPolar(Number.one, Number.pi, 1)) => }
    parser.doParse("1i0.5pi") should matchPattern { case Success(ComplexPolar(Number.one, Number.piBy2, 1)) => }
    parser.doParse("0ipi") should matchPattern { case Success(ComplexPolar(Number.zero, Number.pi, 1)) => }

    parser.doParse("1+i0") should matchPattern { case Success(ComplexCartesian(Number.one, Number.zero)) => }
    parser.doParse("1+i1") should matchPattern { case Success(ComplexCartesian(Number.one, Number.one)) => }
    parser.doParse("0+i1") should matchPattern { case Success(ComplexCartesian(Number.zero, Number.one)) => }
    parser.doParse("1+ipi") should matchPattern { case Success(ComplexPolar(Number.one, Number.pi, 1)) => }
    parser.doParse("1i0.5pi") should matchPattern { case Success(ComplexPolar(Number.one, Number.piBy2, 1)) => }
    parser.doParse("0+ipi") should matchPattern { case Success(ComplexPolar(Number.zero, Number.pi, 1)) => }

    parser.doParse("1-i0") should matchPattern { case Success(ComplexCartesian(Number.one, Number.zero)) => }
    parser.doParse("1-i1") should matchPattern { case Success(ComplexCartesian(Number.one, Number.negOne)) => }
    parser.doParse("0-i1") should matchPattern { case Success(ComplexCartesian(Number.zero, Number.negOne)) => }
    parser.doParse("1-ipi") should matchPattern { case Success(ComplexPolar(Number.one, Number.minusPi, 1)) => }
//    parser.doParse("1-i0.5pi") should matchPattern { case Success(ComplexPolar(Number.one, negate(Number.piBy2), 1)) => }
    parser.doParse("0-ipi") should matchPattern { case Success(ComplexPolar(Number.zero, Number.minusPi, 1)) => }
  }

}
