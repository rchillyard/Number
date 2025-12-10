package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.core.misc.RandomLong
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class RandomLongSpec extends AnyFlatSpec {

  behavior of "RandomLong"

  it should "get" in {
    val target = RandomLong(0L)
    target.get shouldBe 0L
    target.next.get shouldBe -4962768465676381896L
  }


  it should "map" in {
    val target = RandomLong(0L)
    val z = target.map(x => x.toString)
    z.get shouldBe "0"
//        z.next.get shouldBe "-4962768465676381896L"
  }

}
