/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.inner.RationalException
import com.phasmidsoftware.number.core.misc.FP
import com.phasmidsoftware.number.core.misc.FP._
import com.phasmidsoftware.number.core.numerical.CoreException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow
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

  behavior of "doMap function"
  it should "doMap: return Success(Z) when given Right(R)" in {
    val r2Z: Int => String = _.toString
    val l2Zy: String => Try[String] = _ => Failure(new RuntimeException("Should not be called"))
    doMap(Right(42))(r2Z, l2Zy) shouldBe Success("42")
  }

  it should "doMap: call tryMapLeft and return its result when given Left(L)" in {
    val r2Z: Int => String = _.toString
    val l2Zy: String => Try[String] = s => Failure(new RuntimeException(s"Handled: $s"))
    doMap(Left("Error"))(r2Z, l2Zy) shouldBe a[Failure[_]]
    doMap(Left("Error"))(r2Z, l2Zy).failed.get.getMessage shouldBe "Handled: Error"
  }

  it should "resource" in {
    resource[FPSpec]("/carmichael.txt") should matchPattern { case Success(_) => }
  }

  it should "toTryWithThrowable" in {
    toTryWithThrowable(Some(1), noneException) shouldBe Success(1)
    toTryWithThrowable(None, noneException) shouldBe failure
  }

  // TODO try to understand why this doesn't work for CircleCI (actually, it doesn't work locally either)
  it should "readFromResource" taggedAs Slow in {
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

  it should "fail1: return a Failure with CoreException when given a String" in {
    val s = "Test Error"
    val failureFunction = FP.fail[String, Double](s) // Specify the types explicitly X is String, Z is Double
    val result: Try[Double] = failureFunction("anyString") // Passing a String, but ignored

    result shouldBe a[Failure[_]]
    val value1 = result.failed.get // This is fine in a Spec file.
    value1 shouldBe a[CoreException]
    value1.getMessage shouldBe s
  }

  it should "fail2: return a Failure with the given Throwable" in {
    val e = CoreException("Test Error")
    val result: Try[Int] = FP.fail[String, Int](e)("anything")

    result shouldBe a[Failure[_]]
    result.failed.get shouldBe e
    result.failed.get.getMessage shouldBe "Test Error"
  }

  it should "getOrThrow" in {
    getOrThrow(Some(1), noneException) shouldBe 1
    an[Exception] should be thrownBy getOrThrow(None, noneException)
  }

  it should "transpose" in {
    // Define an implicit conversion from R to L
    implicit val intToString: Int => String = _.toString

    // Convert Right(R) into Left(L) using implicit conversion
    transpose(Right(42))(intToString) shouldBe Left("42")
    // Keep Left(L) unchanged
    transpose(Left("Error"))(intToString) shouldBe Left("Error")
  }

  behavior of "toTry"

  private val hello = "hello"

  it should "toTry" in {
    toTry(Some(1), Failure(new NoSuchElementException)) should matchPattern { case Success(1) => }
    toTry(None, Failure(new NoSuchElementException)) should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  it should "toTryWithThrowable" in {
    toTryWithThrowable(Some(1), RationalException(hello)) should matchPattern { case Success(1) => }
    toTryWithThrowable(None, RationalException(hello)) should matchPattern { case Failure(RationalException(`hello`)) => }
  }

  it should "toTryWithRationalException" in {
    toTryWithRationalException(Some(1), hello) should matchPattern { case Success(1) => }
    toTryWithRationalException(None, hello) should matchPattern { case Failure(RationalException(`hello`)) => }
  }

}
