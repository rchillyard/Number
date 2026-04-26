package com.phasmidsoftware.number.core.numerical

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbsoluteFuzzRenderSpec extends AnyFlatSpec with Matchers {

  behavior of "AbsoluteFuzz.getQualifiedString"

  // Table-driven tests based on agreed truth table:
  // value  | magnitude | magnitudeExp | fuzzDigits | decimalPlaces | expected
  // 100.0  | 0.5       | -1           | 1          | 1             | "100.0(5)"
  // 100.0  | 0.19      | -1           | 2          | 2             | "100.00(19)"
  // 100.0  | 0.25      | -1           | 2          | 2             | "100.00(25)"
  // 100.0  | 0.299     | -1           | 2          | 2             | "100.00(30)"
  // 100.0  | 0.30      | -1           | 1          | 1             | "100.0(3)"
  // 2.71828| 0.0815    | -2           | 1          | 2             | "2.72(8)"
  // 67.65  | 1.353     | 0            | 2          | 2             | "6.77[14]E+01"

  it should "render 100.0 with magnitude 0.5 (leading digit 5, 1 fuzz digit)" in {
    val fuzz = AbsoluteFuzz(0.5, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(100.0)
    embedded shouldBe true
    str shouldBe "100.0(5)"
  }

  it should "render 100.0 with magnitude 0.19 (leading digit 1, 2 fuzz digits)" in {
    val fuzz = AbsoluteFuzz(0.19, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(100.0)
    embedded shouldBe true
    str shouldBe "100.00(19)"
  }

  it should "render 100.0 with magnitude 0.25 (leading digit 2, 2 fuzz digits)" in {
    val fuzz = AbsoluteFuzz(0.25, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(100.0)
    embedded shouldBe true
    str shouldBe "100.00(25)"
  }

  it should "render 100.0 with magnitude 0.299 (leading digit 2, rounds up to 30)" in {
    pending // Issue #203
    val fuzz = AbsoluteFuzz(0.299, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(100.0)
    embedded shouldBe true
    str shouldBe "100.00(30)"
  }

  it should "render 100.0 with magnitude 0.30 (leading digit 3, 1 fuzz digit)" in {
    val fuzz = AbsoluteFuzz(0.30, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(100.0)
    embedded shouldBe true
    str shouldBe "100.0(3)"
  }

  it should "render 2.71828 with magnitude 0.0815 (leading digit 8, 1 fuzz digit)" in {
    pending // Issue #203
    val fuzz = AbsoluteFuzz(0.0815, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(2.71828)
    embedded shouldBe true
    str shouldBe "2.72(8)"
  }

  it should "render 67.65 with magnitude 1.353 (leading digit 1, 2 fuzz digits, scientific)" in {
    pending // Issue #203
    val fuzz = AbsoluteFuzz(1.353, Box)
    val (embedded, str) = fuzz.getQualifiedString(67.65)
    embedded shouldBe true
    println(s"str: $str")
    str shouldBe "6.77[14]E+01"
  }

  it should "render 1.0 with magnitude 0.01 (leading digit 1, 2 fuzz digits)" in {
    pending // Issue #203
    val fuzz = AbsoluteFuzz(0.01, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(1.0)
    embedded shouldBe true
    str shouldBe "1.00(01)"
  }

  it should "render 2.71828 with magnitude 0.0271828 (leading digit 2, 2 fuzz digits)" in {
    val fuzz = AbsoluteFuzz(0.0271828, Gaussian)
    val (embedded, str) = fuzz.getQualifiedString(2.71828)
    embedded shouldBe true
    str shouldBe "2.718(27)"
  }
}