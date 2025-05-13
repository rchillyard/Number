package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Value.{fromInt, fromRational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FactorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Factor"

  it should "add" in {
    PureNumber.add(fromInt(1), fromInt(2), PureNumber) shouldBe Some((fromInt(3), PureNumber, None))
    PureNumber.add(fromInt(1), fromInt(2), Radian) shouldBe None
    PureNumber.add(fromInt(1), fromInt(2), SquareRoot) shouldBe None
    PureNumber.add(fromInt(1), fromInt(2), NatLog) shouldBe None
  }

  it should "multiply PureNumber by Other" in {
    PureNumber.multiply(fromInt(3), fromInt(2), PureNumber) shouldBe Some((fromInt(6), PureNumber, None))
    PureNumber.multiply(fromInt(2), fromInt(3), Radian) shouldBe Some((fromInt(6), Radian, None))
    PureNumber.multiply(fromInt(3), fromInt(2), SquareRoot) shouldBe Some((fromInt(18), SquareRoot, None))
    PureNumber.multiply(fromInt(3), fromInt(2), CubeRoot) shouldBe Some((fromInt(54), CubeRoot, None))
    PureNumber.multiply(fromInt(4), fromInt(2), Log2) shouldBe Some((fromInt(4), Log2, None))
    PureNumber.multiply(fromInt(3), fromInt(2), Log2) shouldBe None
  }

  it should "multiply Other by PureNumber" in {
    Radian.multiply(fromInt(2), fromInt(3), PureNumber) shouldBe Some((fromInt(6), Radian, None))
    SquareRoot.multiply(fromInt(2), fromInt(3), PureNumber) shouldBe Some((fromInt(18), SquareRoot, None))
    CubeRoot.multiply(fromInt(2), fromInt(3), PureNumber) shouldBe Some((fromInt(54), CubeRoot, None))
    Log2.multiply(fromInt(2), fromInt(4), PureNumber) shouldBe Some((fromInt(4), Log2, None))
    Log2.multiply(fromInt(2), fromInt(3), PureNumber) shouldBe None
  }

  it should "multiply Other by Log" in {
    NatLog.multiply(fromInt(2), fromInt(4), NatLog) shouldBe Some((fromInt(6), NatLog, None))
    Radian.multiply(fromInt(2), fromInt(3), NatLog) shouldBe None
    SquareRoot.multiply(fromInt(2), fromInt(3), NatLog) shouldBe None
    SquareRoot.multiply(fromInt(2), fromInt(2), Log2) shouldBe Some((fromInt(32), SquareRoot, None))
    CubeRoot.multiply(fromInt(2), fromInt(3), NatLog) shouldBe None
    Log2.multiply(fromInt(2), fromInt(4), Log2) shouldBe Some((fromInt(6), Log2, None))
    Log2.multiply(fromInt(2), fromInt(3), Log2) shouldBe Some((fromInt(5), Log2, None))
  }

  it should "multiply Other by Root" in {
    val maybeNumber1 = PureNumber.multiply(fromInt(-1), fromRational(Rational(3).invert), SquareRoot)
    maybeNumber1 shouldBe None
    val maybeNumber2 = SquareRoot.multiply(fromRational(Rational(3).invert), fromInt(-1), SquareRoot)
    maybeNumber2 shouldBe None
    SquareRoot.multiply(fromInt(2), fromInt(3), SquareRoot) shouldBe Some((fromInt(6), SquareRoot, None))
    SquareRoot.multiply(fromInt(2), fromInt(2), SquareRoot) shouldBe Some((fromInt(2), PureNumber, None))
    CubeRoot.multiply(fromInt(2), fromInt(3), SquareRoot) shouldBe Some((fromInt(108), AnyRoot(6), None))
  }

  it should "raise" in {
    PureNumber.raise(fromInt(3), fromInt(2), PureNumber) shouldBe Some((fromInt(9), PureNumber, None))
    SquareRoot.raise(fromInt(3), fromInt(2), PureNumber) shouldBe Some((fromInt(3), PureNumber, None))
    PureNumber.raise(fromInt(3), fromInt(3), PureNumber) shouldBe Some((fromInt(27), PureNumber, None))
    CubeRoot.raise(fromInt(3), fromInt(3), PureNumber) shouldBe Some((fromInt(3), PureNumber, None))
    PureNumber.raise(fromInt(3), fromInt(4), PureNumber) shouldBe Some((fromInt(81), PureNumber, None))
    AnyRoot(4).raise(fromInt(3), fromInt(4), PureNumber) shouldBe Some((fromInt(3), PureNumber, None))
    PureNumber.raise(fromInt(2), fromRational(Rational.half), PureNumber) shouldBe Some((fromInt(2), SquareRoot, None))
  }

  it should "clean" in {
    SquareRoot.clean(Some((fromInt(9), SquareRoot, None))) shouldBe Some((fromInt(3), PureNumber, None))
    SquareRoot.clean(Some((fromInt(36), AnyRoot(4), None))) shouldBe Some((fromInt(6), SquareRoot, None))
  }

  it should "convert" in {
    SquareRoot.convert(fromInt(4), CubeRoot) shouldBe Some(fromInt(8))

  }

  it should "isAdditive" in {

  }

  it should "isA" in {
    PureNumber.isA(RestrictedContext(PureNumber)) shouldBe true
    PureNumber.isA(AnyContext) shouldBe true
  }

  it should "value" in {

  }

  it should "render" in {
    val z: Option[ProtoNumber] = CubeRoot.multiply(fromInt(2), fromInt(3), SquareRoot)
    val p = Number.one
    val q: Option[Number] = z map GeneralNumber.protoNumberFunction(p)
    q map (_.render) shouldBe Some("108^(1/6)")
  }

  it should "sPiAlt2" in {

  }

  it should "sPiAlt1" in {

  }

  it should "sPiAlt0" in {

  }

  it should "apply" in {

  }

  it should "sPi" in {

  }

  it should "sE" in {

  }

}
