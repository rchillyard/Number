package com.phasmidsoftware.number.problems

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.language.postfixOps
import scala.util.Random

class PotHolesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "PotHoles"

  it should "cost 0" in {
    PotHoles.cost(Nil, Nil) shouldBe 0
  }

  it should "cost 1" in {
    PotHoles.cost(Seq(1), Seq(0)) shouldBe 1
  }

  it should "cost 3" in {
    PotHoles.cost(Seq(1, 2), Seq(0, 0)) shouldBe 3
  }

  it should "cost 8" in {
    PotHoles.cost(Seq(5, 1, 2), Seq(10, 0, 0)) shouldBe 8
  }

  it should "cost 15" in {
    PotHoles.cost(Seq(5, 1, 2, 17), Seq(10, 0, 0, 10)) shouldBe 15
  }

  it should "cost 16" in {
    PotHoles.cost(Seq(5, 1, 2, 17, 15), Seq(10, 0, 0, 10, 20)) shouldBe 16
  }

  it should "cost 10" in {
    PotHoles.cost(Seq(3, 10, 6, 7, 13), Seq(2, 4, 8, 12, 17)) shouldBe 10
  }

  it should "cost 11" in {
    PotHoles.cost(Seq(3, 10, 6, 7, 13, 21), Seq(2, 4, 8, 12, 17, 20)) shouldBe 11
  }

  it should "cost random" in {
    val r = new Random(0L)

    val potholes: Seq[Double] = LazyList.continually(r.nextInt(10000)) take 1000 map (x => x.toDouble) toList
    val crews: Seq[Double] = LazyList.continually(r.nextInt(10000)) take 1000 map (x => x.toDouble) toList

    PotHoles.cost(potholes, crews) shouldBe 195540
  }
}
