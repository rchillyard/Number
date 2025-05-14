package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.misc.Bumperator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BumperatorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Bumperator"

  it should "hasNext 0" in {
    val bumperator = Bumperator[Int](Nil)(_ + _ == 0)
    bumperator.hasNext shouldBe false
  }
  it should "hasNext 1" in {
    val target = Bumperator(Seq(1))(_ + _ == 0)
    target.hasNext shouldBe true
    target.next() shouldBe 1
    target.hasNext shouldBe false
  }
  it should "hasNext 2" in {
    val target = Bumperator(Seq(1, 2))(_ + _ == 0)
    target.hasNext shouldBe true
    target.next() shouldBe 1
    target.hasNext shouldBe true
    target.next() shouldBe 2
    target.hasNext shouldBe false
  }

  it should "next 1" in {
    val target = Bumperator(List(1, 2, 3, 4))(_ + _ == 0)
    target.hasNext shouldBe true
    target.next() shouldBe 1
    target.hasNext shouldBe true
    target.next() shouldBe 2
    target.hasNext shouldBe true
    target.next() shouldBe 3
    target.hasNext shouldBe true
    target.next() shouldBe 4
    target.hasNext shouldBe false
  }

  it should "toList 1" in {
    Bumperator(List(1, 2, 3, 4))(_ + _ == 0).toList shouldBe List(1, 2, 3, 4)
    Bumperator(List(-1, 1, 2, 3))(_ + _ == 0).toList shouldBe List(2, 3)
    Bumperator(List(0, -1, 1, 2))(_ + _ == 0).toList shouldBe List(0, 2)
    Bumperator(List(0, 2, -1, 1))(_ + _ == 0).toList shouldBe List(0, 2)
  }

}
