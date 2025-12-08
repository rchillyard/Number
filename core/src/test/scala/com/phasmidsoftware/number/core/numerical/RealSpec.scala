package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.core.expression.{Expression, Literal}
import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import com.phasmidsoftware.number.core.inner.{PureNumber, Radian, Rational, SquareRoot}
import com.phasmidsoftware.number.core.numerical.ComplexPolar.±
import com.phasmidsoftware.number.core.numerical.Number.{negate, zeroR}
import com.phasmidsoftware.number.core.numerical.Real.RealIsOrdering
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RealSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Real => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  private val one = Constants.one
  private val bigOne = BigInt(1)
  private val doubleOne = 1.0

  behavior of "create"
  it should "yield Right(1)" in {
    val target = Real(1)
    target should matchPattern { case Real(ExactNumber(_, _)) => }
    target.x.nominalValue should matchPattern { case Right(_) => }
  }
  it should "yield Right(1, Radian)" in {
    val target: Real = Constants.pi
    target should matchPattern { case Real(ExactNumber(_, _)) => }
    target.x.nominalValue should matchPattern { case Right(_) => }
    target.x.factor shouldBe Radian
  }

  behavior of "toString"
  it should "yield 1" in {
    val target = Real.apply(1)
    target.toString shouldBe "1"
  }
  it should "work for Pi" in {
    Constants.pi.toString shouldBe "\uD835\uDED1"
  }
  it should "work for E" in {
    Constants.e.toString shouldBe "\uD835\uDF00"
  }
  it should "work for E∧2" in {
    val target = Constants.e power Real(2)
    target.toString shouldBe "\uD835\uDF00\u00B2"
  }
  it should "work for E∧3" in {
    val target = Constants.e power 3
    target.toString shouldBe "\uD835\uDF00\u00B3"
  }
  it should "work for E∧4" in {
    val target = Constants.e power 4
    target.toString shouldBe "\uD835\uDF00\u2074"
  }
  it should "work for E∧10" in {
    val target = Constants.e power 10
    target.toString shouldBe "\uD835\uDF00∧10"
  }

  behavior of "render"
  it should "work for square root E" in {
    val target = Constants.e.sqrt
    target.render shouldBe "±√\uD835\uDF00"
  }

  behavior of "toDouble"
  it should "yield 1" in {
    val target = Real(1)
    target.toDouble shouldBe doubleOne
  }

  behavior of "apply"
  it should """work for "1"""" in {
    val target = Real("1")
    target.x.nominalValue shouldBe Right(1)
  }
  it should """work for "𝛑" """ in {
    val target = Real("\uD835\uDED1")
    target.isExact shouldBe true
    target shouldBe Constants.pi
  }
  it should "work for 1" in {
    val target = Real(1)
    target.isExact shouldBe true
  }

  behavior of "asReal"

  it should "work for real" in {
    val x = Constants.one
    x.asReal shouldBe Some(Constants.one)
  }
  it should "work for number" in {
    val x = Number.one
    x.asReal shouldBe Some(Constants.one)
  }

  behavior of "plus"
  it should "add 1 and 2" in {
    val x = Constants.one
    val y = Constants.two
    (x add y) shouldBe Real(3)
  }
  it should "add 1 to pi" in {
    val x1 = Constants.one
    val x2 = Constants.pi
    (x1 add x2).toString shouldBe "4.1415926535897930(41)"
  }
  it should "add 1 to e" in {
    val x1 = Constants.one
    val x2 = Constants.e
    (x1 add x2) should ===(Real(3.7182818284590450))
  }

  behavior of "minus"
  it should "negate 1" in {
    val x = Constants.one
    -x shouldBe Real(-1)
  }

  behavior of "subtract"
  it should "subtract 1 from 2" in {
    val x = Real(2)
    val y = one
    (x add -y) shouldBe Constants.one
  }

  behavior of "multiply"
  it should "multiply 1 and 2" in {
    val x = Constants.one
    val y = Constants.two
    (x multiply y) shouldBe Real(2)
  }

  behavior of "invert"
  it should "invert 1" in {
    val x = Constants.one
    x.invert shouldBe x
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = Constants.one
    val y = Constants.two
    (x divide y) shouldBe Real(Number(Rational.half))
  }

  behavior of "power"
  it should "work for squaring PureNumber" in {
    val target = Constants.two
    target.power(2) shouldBe Real(4)
  }
  it should "work for squaring SquareRoot" in {
    val target = Real(Number.root2)
    target.power(2) isSame Real(Number.two)
  }

  behavior of "sqrt"
  it should "work for easy ints" in {
//    val sqrt = Real(1).sqrt
//    sqrt shouldBe ±(1)
    Real(4).sqrt shouldBe ±(2)
    Real(9).sqrt shouldBe ±(3)
  }

  behavior of "sin"
  it should "be zero for pi" in {
    val target = Constants.pi
    target.sin shouldBe Constants.zero
  }
  it should "work for 0" in {
    val target = Real(zeroR)
    target.sin shouldBe Constants.zero
  }
  it should "work for Radian/2" in {
    val target = Real(Number(Rational.half, Radian))
    val sin = target.sin
    sin shouldBe Constants.one
  }
  it should "work for Radian/6" in {
    val target = Real(Number(Rational(6).invert, Radian))
    target.sin shouldBe Real(Number(Rational(1, 2), PureNumber))
  }
  it should "work for Radian/3" in {
    val target = Number(Rational(1, 3), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational(3, 4), SquareRoot)
  }
  it should "work for Radian/4" in {
    val target = Number(Rational(1, 4), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational.half, SquareRoot)
  }
  it should "work for One" in {
    val target = Number.one
    val sin = target.sin
    import com.phasmidsoftware.number.core.numerical.Number.FuzzOps
    sin should ===(0.8414709848078965 ~ 21)
  }

  behavior of "cos"
  it should "be zero for pi" in {
    val target = Number.pi
    target.cos === Number(-1)
  }
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.cos shouldBe Number(1)
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Radian)
    target.cos.isZero shouldBe true
  }
  it should "work for Pi/3" in {
    val target = Number.pi doDivide 3
    target.cos shouldBe Number(Rational(1, 2), PureNumber)
  }
  it should "work for Pi/6" in {
    val target = Number.pi doDivide 6
    target.cos should ===(Number(3).sqrt doDivide 2)
  }

  behavior of "tan"
  it should "be zero for 0" in {
    val target = Number(0, Radian)
    target.tan.isZero shouldBe true
  }
  it should "be zero for pi" in {
    val target = Number.pi
    target.tan.isZero shouldBe true
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Radian)
    target.tan.isInfinite shouldBe true
  }
  it should "work for Pi/3" in {
    val target = Number.pi doDivide 3
    target.tan shouldEqual Number(3).sqrt
  }
  it should "work for Pi/6" in {
    val target = Number.pi doDivide 6
    target.tan should ===(Number(3).sqrt.getInverse)
  }

  behavior of "atan"
  it should "be 0Pi for 0/1" in {
    val target = Number.one
    target.atan(Number.zero) shouldBe Number(0, Radian)
  }
  it should "be pi/4 for 1/1" in {
    val target = Number.one
    target.atan(Number.one) === (Number.pi doDivide 4)
  }
  it should "be 0Pi for 0/-1" in {
    val target = Number.negate(Number.one)
    target.atan(Number.zero) shouldBe Number(1, Radian)
  }
  it should "be Pi / 3 for root(3)" in {
    Number.one.atan(Number(Rational(3)).sqrt) shouldEqual Number(r"1/3", Radian)
  }
  it should "be 7 Pi / 6 for 1/-root(3)" in {
    // CONSIDER shouldn't this be 5 pi / 6?
    negate(Number(Rational(3)).sqrt).atan(Number.one) shouldEqual Number(r"-5/6", Radian)
  }
  it should "be 11 Pi / 6 for -1/2" in {
    Number(Rational(3)).sqrt.atan(negate(Number.one)) shouldEqual Number(r"-1/6", Radian)
  }
  it should "be 3 pi / 4 for 1/-1" in {
    val adjacent = Number.negate(Number.one)
    val opposite = Number.one
    val actual: Number = adjacent.atan(opposite)
    val expected: Number = (Number.pi doMultiply 3) doDivide 4
    // TODO revert this so that it reads actual ... expected
    //  XXX  actual should ===(expected)
    actual shouldBe expected
  }
  // TODO need to operate appropriately on negZero.
  it should "evaluate atan of 1 over -0" in {
    val number = Number.negZero.atan(Number.one)
    number shouldBe Number(Rational(-1, 2), Radian)
  }
  it should "evaluate atan of -1 over 0" in {
    val number = Number.zero.atan(negate(Number.one))
    number shouldBe Number(Rational(-1, 2), Radian)
  }

  behavior of "exp"
  it should "be E for 1" in {
    val target = Constants.one
    target.exp shouldBe Real(Number.e)
  }
  it should "be 1 for 0" in {
    val target = Constants.zero
    target.exp shouldBe one
  }
  it should "be e∧2 for 2" in {
    val target = Number.two
    target.exp should ===(Expression(Constants.e) * Constants.e)
  }

  behavior of "ln"
  it should "be 1 for E" in {
    val target = Real(Number.e)
    target.ln shouldBe one
  }
  it should "be 0 for 1" in {
    val target = Constants.one
    val log = target.ln
    log shouldBe Constants.zero
  }
  it should "be 2 for E∧2" in {
    val target: Number = Expression(Constants.e) * Constants.e
    target.ln should ===(Number.two)
  }

  // NOTE: Following are tests of Ordering[Real]
  private val no: Ordering[Real] = RealIsOrdering

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = one
    val y = one
    no.compare(x, y) shouldBe 0
  }
  it should "work for 1, 2" in {
    val x = one
    val y = Real(2)
    no.compare(x, y) shouldBe -1
  }
  it should "work for 2, 1" in {
    val x = Real(2)
    val y = one
    no.compare(x, y) shouldBe 1
  }

  // NOTE: Following are tests of Numeric[Real]
  private val nn: Numeric[Real] = implicitly[Numeric[Real]]

  behavior of "Numeric toInt"
  it should "work for 1" in {
    val target = one
    nn.toInt(target) shouldBe 1
  }
  it should "work for BigInt 1" in {
    val target = Real(Number(bigOne))
    nn.toInt(target) shouldBe 1
  }
  it should "work for 1.0" in {
    val target = Real(doubleOne)
    nn.toInt(target) shouldBe 1
  }

  behavior of "Numeric toLong"
  it should "work for 1" in {
    val target = one
    nn.toLong(target) shouldBe 1L
  }
  it should "work for 1.0" in {
    val target = Real(doubleOne)
    nn.toLong(target) shouldBe 1L
  }

  behavior of "Numeric toDouble"
  it should "work for 1" in {
    val target = one
    nn.toDouble(target) shouldBe 1.0
  }

  behavior of "Numeric plus"
  it should "work for 1" in {
    val target = one
    nn.plus(target, Constants.two) shouldBe Real(3)
  }
  it should "work for 1.0" in {
    val target = Real(doubleOne)
    nn.plus(target, Constants.one) shouldBe Constants.two
  }

  behavior of "Numeric minus"
  it should "work for 1" in {
    val target = one
    nn.minus(target, Constants.two) shouldBe Real(Number.negOne)
  }
  it should "work for 1.0" in {
    val target = Real(doubleOne)
    nn.minus(target, Constants.one) shouldBe Constants.zero
  }

  behavior of "Numeric fromInt"
  it should "work for -1" in {
    nn.fromInt(-1) shouldBe Real(Number.negOne)
  }
  it should "work for 0" in {
    nn.fromInt(0) shouldBe Constants.zero
  }
  it should "work for 1" in {
    nn.fromInt(1) shouldBe Constants.one
  }

  behavior of "Numeric parseString"
  it should "work for -1" in {
    nn.parseString("-1") shouldBe Some(Real(Number.negOne))
  }
  it should "work for 0" in {
    nn.parseString("0") shouldBe Some(Constants.zero)
  }
  it should "work for 1" in {
    nn.parseString("1") shouldBe Some(Constants.one)
  }
  it should "work for 6.67430(15)E-11" in {
    nn.parseString("6.67430(15)E-11") shouldBe Some(Constants.G)
  }

  // NOTE: Following are tests of Fractional[Real]
  private val nf: Fractional[Real] = implicitly[Fractional[Real]]

  behavior of "Real div"
  it should "work for 1/2" in {
    val target = one
    nf.div(target, Constants.two) shouldBe Real(Number.half)
  }
  it should "work for 2/2" in {
    val target = Constants.two
    nf.div(target, Constants.two) shouldBe Constants.one
  }
  it should "work for 2/3" in {
    val target = Constants.two
    import Rational.RationalOps
    nf.div(target, Real(3)) shouldBe Real(Number(2 :/ 3))
  }

  behavior of "RealOps"

  import Real.RealOps

  it should "work for 2 + Real(3)" in {
    val x: Real = 2 + Real(3)
    x shouldBe Real(5)
  }
  it should "work for 2 * Real(3)" in {
    val x: Real = 2 * Real(3)
    x shouldBe Real(6)
  }
  it should "work for 1 :/ 2" in {
    val x: Real = 1 :/ 2
    x shouldBe Real(Number(Rational.half))
  }

  behavior of "multiply"
  it should "work for pure numbers" in {
    Real(Number.root2) multiply Real(Number.root2) shouldBe Constants.two
    Constants.two multiply Constants.two shouldBe Real(4)
  }
}
