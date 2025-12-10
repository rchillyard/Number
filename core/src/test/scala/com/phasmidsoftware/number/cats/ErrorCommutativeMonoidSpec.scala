package com.phasmidsoftware.number.cats

import cats.syntax.semigroup._
import com.phasmidsoftware.number.cats.ErrorCommutativeMonoid._
import com.phasmidsoftware.number.core.inner.{PureNumber, Value}
import com.phasmidsoftware.number.core.misc.Benchmark._
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, FuzzyNumber, Gaussian, Number, RelativeFuzz}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Usage-focused examples for the error metric Monoid instances.
  *
  * These tests are intentionally lightweight demonstrations rather than law checks.
  */
class ErrorCommutativeMonoidSpec extends AnyFlatSpec with Matchers {

  behavior of "Abstracting advocacy communication into lawful scalar folding"

  it should "match decoupled parallel error folding with direct Number addition (all addition)" taggedAs Slow in {
    implicit val ec: ExecutionContext = ExecutionContext.global

    // Build many fuzzy addends: same nominal 1.2 with absolute Gaussian sigma 0.05
    val terms: List[Number] = List.fill(2000) {
      FuzzyNumber(Value.fromDouble(Some(1.2)), PureNumber, Some(AbsoluteFuzz(0.05, Gaussian)))
    }

    // 1) Traditional: add fuzzy Numbers directly (measure time)
    val (accumulated: Number, tSeqMs) = 1.times {
      terms.tail.foldLeft(terms.head)(_ doAdd _)
    }

    val actualNominal = accumulated.toNominalDouble.getOrElse(Double.NaN)
    val actualAbs = accumulated match {
      case f: FuzzyNumber => f.fuzz.collect { case AbsoluteFuzz(m: Double, Gaussian) => m }.getOrElse(Double.NaN)
      case _ => Double.NaN
    }

    // 2) Decoupled parallel: nominal sum and sigma folding run independently
    val ((decoupledNominal, decoupledSigma), tParMs) = 1.times {
      val fNominal: Future[Double] = Future {
        terms.flatMap(_.toNominalDouble).sum
      }
      val fSigma: Future[Double] = Future {
        val sigmas = terms.map {
          case f: FuzzyNumber => f.fuzz.collect { case AbsoluteFuzz(m: Double, Gaussian) => m }.getOrElse(0.0)
          case _ => 0.0
        }
        sigmas.foldLeft(AbsSigma.zero)(_ |+| AbsSigma(_)).value
      }
      val a = Await.result(fNominal, 5.seconds)
      val b = Await.result(fSigma, 5.seconds)
      (a, b)
    }

    val decoupled: Number = FuzzyNumber(Value.fromDouble(Some(decoupledNominal)), PureNumber, Some(AbsoluteFuzz(decoupledSigma, Gaussian)))

    val decoupledAbs = decoupled match {
      case f: FuzzyNumber => f.fuzz.collect { case AbsoluteFuzz(m: Double, Gaussian) => m }.getOrElse(Double.NaN)
      case _ => Double.NaN
    }

    // Compare nominal and absolute sigma
    actualNominal shouldBe decoupledNominal +- 1e-9
    actualAbs shouldBe decoupledAbs +- 1e-9

    // And new method should be faster (or equal) than traditional
    // NOTE: simple wall-clock comparison; if flaky in CI, relax or increase data size
    // tParMs: 72.449458, tSeqMs: 194.883
    println(s"tParMs: $tParMs, tSeqMs: $tSeqMs")
    assert(tParMs <= tSeqMs, s"decoupled parallel folding should be faster: par=${tParMs}ms vs seq=${tSeqMs}ms")
  }

  // CONSIDER this test is slow. We might want to tag it as Slow (or perhaps try to speed it up)
  it should "match decoupled parallel error folding with direct Number multiplication (all multiplication)" in {
    implicit val ec: ExecutionContext = ExecutionContext.global

    // Build many fuzzy addends: same nominal 1.2 with absolute Gaussian sigma 0.05
    val terms: List[Number] = List.fill(500) {
      FuzzyNumber(Value.fromDouble(Some(1.1)), PureNumber, Some(RelativeFuzz(0.05, Gaussian)))
    }

    // 1) Traditional: multiply fuzzy Numbers directly (measure time)
    val (accumulated: Number, tSeqMs) = 1.times {
      terms.tail.foldLeft(terms.head)(_ doMultiply _)
    }

    val actualNominal = accumulated.toNominalDouble.getOrElse(Double.NaN)
    val actualRel = accumulated match {
      case f: FuzzyNumber => f.fuzz.collect { case RelativeFuzz(m: Double, Gaussian) => m }.getOrElse(Double.NaN)
      case _ => Double.NaN
    }

    // 2) Decoupled parallel: nominal product and sigma folding run independently
    val ((decoupledNominal, decoupledSigma), tParMs) = 1.times {
      val fNominal: Future[Double] = Future {
        val headNominal = terms.headOption.flatMap(_.toNominalDouble).getOrElse(Double.NaN)
        terms.tail.foldLeft(headNominal) { (acc, n) =>
          acc * n.toNominalDouble.getOrElse(1.0)
        }
      }
      val fSigma: Future[Double] = Future {
        // Sequential combination, relative basis propagation: r_xy^2 = r_x^2 + r_y^2 + r_x r_y
        def combineRel(r1: Double, r2: Double): Double = {
          val a = r1
          val b = r2
          math.sqrt(a * a + b * b + a * b)
        }

        val sigmas = terms.map {
          case f: FuzzyNumber => f.fuzz.collect { case RelativeFuzz(m: Double, Gaussian) => m }.getOrElse(0.0)
          case _ => 0.0
        }
        sigmas.foldLeft(0.0)(combineRel)
      }
      val a = Await.result(fNominal, 5.seconds)
      val b = Await.result(fSigma, 5.seconds)
      (a, b)
    }

    val decoupled: Number = FuzzyNumber(Value.fromDouble(Some(decoupledNominal)), PureNumber, Some(RelativeFuzz(decoupledSigma, Gaussian)))

    val decoupledRel = decoupled match {
      case f: FuzzyNumber => f.fuzz.collect { case RelativeFuzz(m: Double, Gaussian) => m }.getOrElse(Double.NaN)
      case _ => Double.NaN
    }

    // Compare nominal and relative sigma
    math.abs(actualNominal - decoupledNominal) / math.abs(actualNominal) should be < 1e-12
    actualRel shouldBe decoupledRel +- 1e-1

    // And new method should be faster (or equal) than traditional
    // NOTE: simple wall-clock comparison; if flaky in CI, relax or increase data size
    // tParMs: 5.240125, tSeqMs: 7731.776417
    println(s"tParMs: $tParMs, tSeqMs: $tSeqMs")
    assert(tParMs <= tSeqMs, s"decoupled parallel folding should be faster: par=${tParMs}ms vs seq=${tSeqMs}ms")
  }
}


