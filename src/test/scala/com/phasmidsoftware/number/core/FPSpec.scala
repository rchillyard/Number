package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util._

class FPSpec extends AnyFlatSpec with should.Matchers {

  behavior of "FP"

  private val noneException = new Exception("None")
  private val failure = Failure(noneException)

  it should "identityTry" in {
    identityTry(1) shouldBe Success(1)
  }

  it should "tryMap" in {
    // TESTME tryMap
  }

  it should "toTry" in {
    toTry(Some(1), failure) shouldBe Success(1)
    toTry(None, failure) shouldBe failure
  }

  it should "optionMap" in {
    // TESTME optionMap
  }

  it should "tryF1" in {
    // TESTME tryF
  }

  it should "tryF2" in {
    // TESTME tryF

  }

  it should "recover" in {
    recover(Some(1), new Exception("logic error")) shouldBe 1
    an[Exception] should be thrownBy recover(None, new Exception("logic error"))
  }

  it should "doMap" in {
    // TESTME doMap
  }

  it should "resource" in {
    resource[FPSpec]("/carmichael.txt") should matchPattern { case Success(_) => }
  }

  it should "toTryWithThrowable" in {
    toTryWithThrowable(Some(1), noneException) shouldBe Success(1)
    toTryWithThrowable(None, noneException) shouldBe failure
  }

  // TODO try to understand why this doesn't work for CircleCI
  ignore should "readFromResource" in {
    val result: Try[Seq[BigInt]] = readFromResource("/carmichael.txt", wa => wa.lastOption)
    result.isSuccess shouldBe true
    result.get.contains(BigInt(530881)) shouldBe true
  }

  it should "optional" in {
    optional[Int](x => x > 0)(1) shouldBe Some(1)
    optional[Int](x => x > 0)(0) shouldBe None
  }

  it should "resourceForClass" in {
    resourceForClass("/carmichael.txt") should matchPattern { case Success(_) => }
  }

  it should "sequence1" in {
    sequence(List(Some(1))) should matchPattern { case Some(List(1)) => }
    sequence(List(Some(1), None)) should matchPattern { case None => }
    sequence(List(None)) should matchPattern { case None => }
  }

  it should "sequence2" in {
    sequence(List(Some(1)).iterator) should matchPattern { case Some(_) => }
    sequence(List(Some(1), None).iterator) should matchPattern { case None => }
    sequence(List(None).iterator) should matchPattern { case None => }
  }

  it should "sequence3" in {
    sequence(List(Success(1)).iterator) should matchPattern { case Success(_) => }
    sequence(List(Success(1), failure).iterator) should matchPattern { case Failure(_) => }
    sequence(List(failure).iterator) should matchPattern { case Failure(_) => }
  }

  it should "sequence4" in {
    sequence(Some(Success(1))) shouldBe Success(Some(1))
  }

  it should "sequence5" in {
    sequence(List(Success(1))) should matchPattern { case Success(List(1)) => }
    sequence(List(Success(1), failure)) should matchPattern { case Failure(_) => }
    sequence(List(failure)) should matchPattern { case Failure(_) => }
  }

  it should "fail1" in {
    // TESTME fail
  }

  it should "fail2" in {
    // TESTME fail
  }

  it should "getOrThrow" in {
    getOrThrow(Some(1), noneException) shouldBe 1
    an[Exception] should be thrownBy getOrThrow(None, noneException)
  }

  it should "transpose" in {
    // TESTME transpose
  }
}
