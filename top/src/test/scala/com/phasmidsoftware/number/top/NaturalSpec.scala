package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.top.Natural.MaxRoman
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NaturalSpec extends AnyFlatSpec with Matchers {

  behavior of "Natural"

  // Basic construction tests
  it should "create Zero" in {
    Zero should be(Zero)
  }

  it should "create natural numbers from Int" in {
    Natural.fromInt(0) should be(Zero)
    Natural.fromInt(1) should be(Successor(Zero))
    Natural.fromInt(3) should be(Successor(Successor(Successor(Zero))))
  }

  it should "throw exception for negative Int" in {
    an[IllegalArgumentException] should be thrownBy Natural.fromInt(-1)
  }

  // Basic operations
  it should "implement next correctly" in {
    Zero.next should be(Successor(Zero))
    Successor(Zero).next should be(Successor(Successor(Zero)))
  }

  it should "add Zero correctly" in {
    val three = Natural.fromInt(3)
    three.add(Zero) should be(three)
    Zero.add(three) should be(three)
  }

  it should "add natural numbers correctly" in {
    val two = Natural.fromInt(2)
    val three = Natural.fromInt(3)
    val five = Natural.fromInt(5)

    two.add(three) should be(five)
    three.add(two) should be(five)
  }

  it should "subtract correctly" in {
    val five = Natural.fromInt(5)
    val three = Natural.fromInt(3)
    val two = Natural.fromInt(2)

    five.subtract(three) should be(Some(two))
    three.subtract(three) should be(Some(Zero))
    two.subtract(three) should be(None)
  }

  it should "multiply correctly" in {
    val three = Natural.fromInt(3)
    val five = Natural.fromInt(5)
    val fifteen = Natural.fromInt(15)

    val natural = three.multiply(five)
    natural should be(fifteen)
    five.multiply(three) should be(fifteen)
    three.multiply(Zero) should be(Zero)
    Zero.multiply(five) should be(Zero)
  }

  behavior of "Roman numeral toString"

  it should "convert Zero to empty string" in {
    Zero.toString should be("")
  }

  it should "convert small numbers to Roman numerals" in {
    Natural.fromInt(1).toString should be("I")
    Natural.fromInt(2).toString should be("II")
    Natural.fromInt(3).toString should be("III")
    Natural.fromInt(4).toString should be("IV")
    Natural.fromInt(5).toString should be("V")
  }

  it should "convert numbers with subtractive notation" in {
    Natural.fromInt(4).toString should be("IV")
    Natural.fromInt(9).toString should be("IX")
  }

  it should "convert larger numbers correctly" in {
    Natural.fromInt(10).toString should be("X")
    Natural.fromInt(25).toString should be("XXV")
    Natural.fromInt(49).toString should be("XLIX")
    Natural.fromInt(50).toString should be("L")
    Natural.fromInt(99).toString should be("XCIX")
    MaxRoman.toString should be("MMMCMXCIX")
  }

  behavior of "RomanParser"

  val parser = new RomanParser

  it should "parse simple Roman numerals" in {
    parser.parseRoman("I").get should be(Natural.fromInt(1))
    parser.parseRoman("II").get should be(Natural.fromInt(2))
    parser.parseRoman("III").get should be(Natural.fromInt(3))
    parser.parseRoman("V").get should be(Natural.fromInt(5))
    parser.parseRoman("X").get should be(Natural.fromInt(10))
  }

  it should "parse Roman numerals with subtractive notation" in {
    parser.parseRoman("IV").get should be(Natural.fromInt(4))
    parser.parseRoman("IX").get should be(Natural.fromInt(9))
    parser.parseRoman("XL").get should be(Natural.fromInt(40))
    parser.parseRoman("XC").get should be(Natural.fromInt(90))
    parser.parseRoman("CD").get should be(Natural.fromInt(400))
    parser.parseRoman("CM").get should be(Natural.fromInt(900))
  }

  it should "parse compound Roman numerals" in {
    parser.parseRoman("VII").get should be(Natural.fromInt(7))
    parser.parseRoman("XIV").get should be(Natural.fromInt(14))
    parser.parseRoman("XXV").get should be(Natural.fromInt(25))
    parser.parseRoman("XLIX").get should be(Natural.fromInt(49))
    parser.parseRoman("LXXXVIII").get should be(Natural.fromInt(88))
    parser.parseRoman("XCIX").get should be(Natural.fromInt(99))
  }

  it should "parse larger Roman numerals" in {
    parser.parseRoman("C").get should be(Natural.fromInt(100))
    parser.parseRoman("D").get should be(Natural.fromInt(500))
    parser.parseRoman("M").get should be(Natural.fromInt(1000))
    parser.parseRoman("MCMXCIV").get should be(Natural.fromInt(1994))
    parser.parseRoman("MMXXIII").get should be(Natural.fromInt(2023))
  }

  it should "fail on invalid Roman numerals" in {
    parser.parseRoman("IIII") shouldBe None
    parser.parseRoman("VV") shouldBe None
    parser.parseRoman("ABC") shouldBe None
  }

  behavior of "Roman extractor"

  it should "extract valid Roman numerals in pattern matching" in {
    "VII" match {
      case Roman(n) => n should be(Natural.fromInt(7))
      case _ => fail("Should have matched")
    }
  }

  it should "extract subtractive notation correctly" in {
    "IV" match {
      case Roman(n) => n should be(Natural.fromInt(4))
      case _ => fail("Should have matched")
    }

    "IX" match {
      case Roman(n) => n should be(Natural.fromInt(9))
      case _ => fail("Should have matched")
    }
  }

  it should "extract compound Roman numerals" in {
    "XXV" match {
      case Roman(n) => n should be(Natural.fromInt(25))
      case _ => fail("Should have matched")
    }

    "XCIX" match {
      case Roman(n) => n should be(Natural.fromInt(99))
      case _ => fail("Should have matched")
    }
  }

  it should "fail to match invalid Roman numerals" in {
    "INVALID" match {
      case Roman(_) => fail("Should not have matched")
      case _ => succeed
    }
  }

  it should "use Roman extractor in more complex patterns" in {
    val result = ("VII", "III") match {
      case (Roman(a), Roman(b)) => Some(a.add(b))
      case _ => None
    }

    result should be(Some(Natural.fromInt(10)))
  }

  behavior of "Roman apply method"

  it should "convert Natural to Roman numeral string" in {
    Roman(Natural.fromInt(7)) should be("VII")
    Roman(Natural.fromInt(25)) should be("XXV")
    Roman(Natural.fromInt(99)) should be("XCIX")
  }

  behavior of "Round-trip conversion"

  it should "round-trip through Roman numerals correctly" in {
    val testValues = List(1, 4, 5, 9, 10, 25, 49, 50, 99, 100, 500, 1_000, 1_994, 2_023, 3_999)

    testValues.foreach { n =>
      val natural = Natural.fromInt(n)
      val roman = natural.toString
      val parsed = parser.parseRoman(roman)
      parsed should be(Some(natural))
    }
  }
}