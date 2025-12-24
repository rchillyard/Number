package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.eager.Nat
import org.scalactic.Prettifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Random

class NatSpec extends AnyFlatSpec with should.Matchers {

  implicit val customPrettifier: Prettifier = Prettifier {
    case myObj: Nat => s"Nat:${myObj.toInt}"
    case other => Prettifier.default(other)
  }

  import Nat.natIsSemiring

  private val random = Random

  behavior of "Nat"

  it should "have correct constant elements" in {
    natIsSemiring.zero shouldBe Nat(0)
    natIsSemiring.one shouldBe Nat(1)
    natIsSemiring.zero.toInt shouldBe 0
    natIsSemiring.one.toInt shouldBe 1
  }

  it should "inc" in {
    natIsSemiring.zero.inc shouldBe natIsSemiring.one
    natIsSemiring.one.inc shouldBe Nat(2)
  }

  it should "render" in {
    natIsSemiring.zero.render shouldBe "0"
    natIsSemiring.one.render shouldBe "1"
    Nat(2).render shouldBe "2"
  }

  it should "fail on negative numbers" in {
    intercept[AssertionError](Nat(-1))
  }

  it should "perform addition correctly" in {
    natIsSemiring.plus(Nat(5), natIsSemiring.zero) shouldBe Nat(5)
    natIsSemiring.plus(natIsSemiring.zero, Nat(5)) shouldBe Nat(5)
    natIsSemiring.plus(Nat(5), Nat(3)) shouldBe Nat(8)
    natIsSemiring.plus(Nat(3), Nat(5)) shouldBe Nat(8)
    val max = 100_000
    val r1 = Nat(random.between(0, max))
    val r2 = Nat(random.between(0, max))
    val expected = Nat(r1.toInt + r2.toInt)
    r1 + r2 shouldBe expected
    r2 + r1 shouldBe expected
  }

  it should "perform multiplication correctly" in {
    natIsSemiring.times(Nat(5), natIsSemiring.one) shouldBe Nat(5)
    natIsSemiring.times(natIsSemiring.one, Nat(5)) shouldBe Nat(5)
    natIsSemiring.times(Nat(4), Nat(2)) shouldBe Nat(8)
    natIsSemiring.times(Nat(2), Nat(4)) shouldBe Nat(8)
    val max = 1000 // NOTE don't try to increase this much because then it takes a long time to build the product Nat.
    val r1 = random.between(0, max)
    val r2 = random.between(0, max)
    val n1 = Nat(r1)
    val n2 = Nat(r2)
    val expected = Nat(r1 * r2)
    natIsSemiring.times(n1, n2) shouldBe expected
    natIsSemiring.times(n2, n1) shouldBe expected
  }

  it should "satisfy distributive property" in {
    val a = Nat(2)
    val b = Nat(3)
    val c = Nat(4)
    val left = natIsSemiring.times(a, natIsSemiring.plus(b, c))
    val right = natIsSemiring.plus(natIsSemiring.times(a, b), natIsSemiring.times(a, c))
    left shouldBe right
  }
}
