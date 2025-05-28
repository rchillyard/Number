package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FuzzySpec extends AnyFlatSpec with should.Matchers {

  behavior of "Fuzzy"

  case class Color(r: Short, g: Short, b: Short) {
    def difference(x: Color): Color = Color(math.abs(r - x.r), math.abs(g - x.g), math.abs(b - x.b))

    def whiteness: Double = math.sqrt(r * r + g * g + b * b) / 255
  }

  object Color {
    def apply(r: Int, g: Int, b: Int): Color = Color(r.toShort, g.toShort, b.toShort)

    trait FuzzyColor extends Fuzzy[Color] {
      def same(p: Double)(x1: Color, x2: Color): (Boolean, Color) = {
        val diff = x2.difference(x1)
        (-math.log(diff.whiteness) / 3 > p) -> diff
      }
    }

    implicit object FuzzyColor extends FuzzyColor
  }

  import Color._

  private val white = Color(255, 255, 255)
  private val red = Color(255, 0, 0)
  private val veryLightBlue = Color(255, 255, 254)
  private val lightPurple = Color(241, 241, 241)
  private val slightlyLighterPurple = Color(242, 242, 242)

  it should "same" in {
    val requiredConfidence = 0.8 // 80% confidence
    val cf = implicitly[Fuzzy[Color]]
    val f: (Color, Color) => Boolean = (a, b) => cf.same(requiredConfidence)(a, b)._1
    f(white, red) shouldBe false
    f(white, veryLightBlue) shouldBe true
    f(white, lightPurple) shouldBe false
    f(white, slightlyLighterPurple) shouldBe true
  }

}
