package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try

class FPSpec extends AnyFlatSpec with should.Matchers {

    behavior of "FP"

    it should "identityTry" in {

    }

    it should "tryMap" in {

    }

    it should "toTry" in {

    }

    it should "optionMap" in {

    }

    it should "tryF1" in {

    }

    it should "tryF2" in {

    }

    it should "recover" in {

    }

    it should "doMap" in {

    }

    it should "resource" in {

    }

    it should "toTryWithThrowable" in {

    }

    it should "readFromResource" in {
        val result: Try[Seq[BigInt]] = FP.readFromResource("/carmichael.txt", wa => wa.lastOption)
        result.isSuccess shouldBe true
        result.get.contains(BigInt(530881)) shouldBe true
    }

    it should "optional" in {

    }

    it should "resourceForClass" in {

    }

    it should "sequence1" in {

    }

    it should "sequence2" in {

    }

    it should "sequence3" in {

    }

    it should "sequence4" in {

    }

    it should "sequence5" in {

    }

    it should "fail1" in {

    }

    it should "fail2" in {

    }

    it should "getOrThrow" in {

    }

    it should "transpose" in {

    }

}
