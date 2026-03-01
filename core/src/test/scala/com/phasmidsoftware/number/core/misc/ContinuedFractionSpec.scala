/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import com.phasmidsoftware.number.core.misc.*
import com.phasmidsoftware.number.core.misc.ConFrac.LongLazyListFrom
import com.phasmidsoftware.number.core.misc.ContinuedFraction.{Hurwitz, fPiBy4Leibniz}
import com.phasmidsoftware.number.core.numerical.Fuzziness.showPercentage
import com.phasmidsoftware.number.core.numerical.Number
import org.scalatest.flatspec
import org.scalatest.matchers.should

class ContinuedFractionSpec extends flatspec.AnyFlatSpec with should.Matchers {

  val goldenRatio: Double = (1 + math.sqrt(5)) / 2

  behavior of "ConFrac.take"

  it should "create singleton simple ConFrac" in {
    val one: LazyList[Long] = LongLazyListFrom(1).take(1)
    val cf = ConFrac.simple(one)
    cf.b shouldBe 1
    cf.tailOption shouldBe None
  }

  it should "create doubleton simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    cf.b shouldBe 1
    val co = cf.tailOption
    co.get.a shouldBe 1
    co.get.c.b shouldBe 2
    co.get.c.tailOption shouldBe None
  }

  behavior of "ConFrac.takeWhile"

  it should "phi ConFrac.simple precise to 1E-6" in {
    val one = LazyList.continually(1L)
    val target = ConFrac.simple(one)
    val epsilon = 1E-6

    def imprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.takeWhile(imprecise)
    cf.reverseCoefficients.length shouldBe 16
    cf.toRational.toDouble shouldBe goldenRatio +- epsilon
  }

  it should "fail to evaluate phi ConFrac precise to 1E-6" in {
    val one = LazyList.continually(1L)
    val target = ConFrac.simple(one).take(5)
    val epsilon = 1E-6

    def imprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.takeWhile(imprecise)
    a[ConFracException] should be thrownBy cf.reverseCoefficients
  }

  behavior of "ConFrac.convergents"

  val lFib: LazyList[Long] = 1L #:: lFib.scanLeft(1L)(_ + _)
  // NOTE: these values are: 1, 2, 3/2, 5/3, 8/5, 13/8, 21/13, 34/21, 55/34, 89/55, 144/89, etc.
  val convergentsPhi: LazyList[Rational] = Pair.zip(lFib, lFib.tail) map (p => p.toRational)

  def tupleMatch(t: (Any, Any)): Boolean = t._1 == t._2

  it should "get convergents from simple ConFrac" in {
    val one: LazyList[Long] = LazyList.continually(1L)
    val target = ConFrac.simple(one)
    target.convergents.take(5).zip(convergentsPhi).forall(tupleMatch) shouldBe true
  }

  it should "get convergents from simple finite expansion for pi" in {
    val target = ConFrac.PiSimple
    val rs = target.convergents.toList
    rs.head shouldBe Rational(3)
    val c1 = rs(1)
    c1 shouldBe r"22/7"
    showPercentage(Number(c1).asComparedWith(Number.pi)) shouldBe "0.02%"
    val c2 = rs(2)
    c2 shouldBe r"333/106"
    showPercentage(Number(c2).asComparedWith(Number.pi)) shouldBe "0.0013%"
    val c3 = rs(3)
    c3 shouldBe r"355/113"
    showPercentage(Number(c3).asComparedWith(Number.pi)) shouldBe "0.0000042%"
    val c4 = rs(4)
    c4 shouldBe r"103993/33102"
    showPercentage(Number(c4).asComparedWith(Number.pi)) shouldBe "0.0000000092%"
    val c5 = rs(5)
    c5 shouldBe r"104348/33215"
    showPercentage(Number(c5).asComparedWith(Number.pi)) shouldBe "0.0000000053%"
    val c6 = rs(6)
    c6 shouldBe r"208341/66317"
    showPercentage(Number(c6).asComparedWith(Number.pi)) shouldBe "0.0000000019%"
    val c7 = rs(7)
    c7 shouldBe r"312689/99532"
    showPercentage(Number(c7).asComparedWith(Number.pi)) shouldBe "0.00000000046%"
    val c8 = rs(8)
    c8.toRationalString shouldBe "833719/265381"
    showPercentage(Number(c8).asComparedWith(Number.pi)) shouldBe "0.00000000014%"
  }

  it should "get convergents for e" in {
    val target = ConFrac.E
    val rs = target.convergents.take(5).toList
    rs.head shouldBe Rational.two
    rs(1) shouldBe Rational(3)
    rs(2) shouldBe r"8/3"
    rs(3) shouldBe r"11/4"
    rs(4) shouldBe r"19/7"
  }

  it should "get convergents for root 2" in {
    val target = ContinuedFraction.root2
    val rs = target.convergents.take(11).toList
    rs.head shouldBe Rational.one
    rs(1) shouldBe r"3/2"
    rs(2) shouldBe r"7/5"
    rs(3) shouldBe r"17/12"
    rs(4) shouldBe r"41/29"
    rs(5) shouldBe r"99/70"
    rs(6) shouldBe r"239/169"
    rs(7) shouldBe r"577/408"
    rs(8) shouldBe r"1393/985"
    rs(9) shouldBe r"3363/2378"
    rs(10) shouldBe r"8119/5741"
  }

  behavior of "ConFrac.coefficients"

  it should "work on finite simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2)
  }

  it should "work on very short finite simple ConFrac" in {
    val one = LongLazyListFrom(1)
    val cf = ConFrac.simple(one).take(0)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail shouldBe LazyList()
  }

  it should "work on infinite simple ConFrac" in {
    val one = LongLazyListFrom(1)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2)
    xs.tail.tail.head shouldBe Pair(3)
  }

  it should "work on finite general ConFrac" in {
    val pairs: LazyList[Pair] = ContinuedFraction.fPiBy4Leibniz
    val cf = ConFrac(pairs)
    val xs: LazyList[Pair] = cf.coefficients
    xs.head shouldBe Pair(1, 0)
    xs.tail.head shouldBe Pair(2, 1)
    xs.tail.tail.head shouldBe Pair(2, 9)
  }

  behavior of "ConFrac.reverseCoefficients"

  it should "work on finite simple ConFrac" in {
    val one = LongLazyListFrom(1).take(2)
    val cf = ConFrac.simple(one)
    val xs: LazyList[Pair] = cf.reverseCoefficients
    xs.head shouldBe Pair(2)
    xs.tail.head shouldBe Pair(1)
  }

  behavior of "ConFrac.toNominalRational"

  it should "implement simple toNominalRational" in {
    // NOTE: these tests approximate the Golden Ratio phi
    val target = ConFrac.simple(LazyList.continually(1))
    target.take(0).toRational shouldBe convergentsPhi.head
    target.take(1).toRational shouldBe convergentsPhi(1)
    target.take(2).toRational shouldBe convergentsPhi(2)
    target.take(3).toRational shouldBe convergentsPhi(3)
    target.take(4).toRational shouldBe convergentsPhi(4)
    target.take(5).toRational shouldBe convergentsPhi(5)
    target.take(6).toRational shouldBe convergentsPhi(6)
    target.take(7).toRational shouldBe convergentsPhi(7)
    target.take(8).toRational shouldBe convergentsPhi(8)
    target.take(9).toRational shouldBe convergentsPhi(9)
    target.take(10).toRational shouldBe convergentsPhi(10)

  }

  it should "implement simple toRationalOption(Rational)" in {
    val cf = ConFrac.simple(LazyList.continually(1))
    cf.toRationalOption(Rational(10).invert).get shouldBe convergentsPhi(3)
    cf.toRationalOption(Rational(25).invert).get shouldBe convergentsPhi(4)
    cf.toRationalOption(Rational(50).invert).get shouldBe convergentsPhi(5)
    cf.toRationalOption(Rational(100).invert).get shouldBe convergentsPhi(6)

  }

  it should "implement simple finite toRationalOption(Rational)" in {
    val cf = ConFrac.simple(LazyList.continually(1)).take(5)
    cf.toRationalOption(Rational(10).invert).get shouldBe convergentsPhi(3)
    cf.toRationalOption(Rational(25).invert).get shouldBe convergentsPhi(4)
    cf.toRationalOption(Rational(50).invert) should matchPattern { case None => }
  }

  it should "implement simple toNominalRational(Double)" in {
    val one: LazyList[Long] = LongLazyListFrom(1)
    val cf: ConFrac = ConFrac.simple(one)

    import Rational.RationalHelper
    cf.toRational(0.05)(Hurwitz) shouldBe Some(r"10/7")
    cf.toRational(0.01)(Hurwitz) shouldBe Some(r"43/30")
    cf.toRational(0.005)(Hurwitz) shouldBe Some(r"43/30")
    cf.toRational(0.001)(Hurwitz) shouldBe Some(r"225/157")
    cf.toRational(0.0001)(Hurwitz) shouldBe Some(r"1393/972")
    cf.toRational(0.000005)(Hurwitz) shouldBe Some(r"9976/6961")
    cf.toRational(0.0000001)(Hurwitz) shouldBe Some(r"81201/56660")
    cf.toRational(0.000000002)(Hurwitz) shouldBe Some(r"740785/516901")
    cf.toRational(0.00000000005)(Hurwitz) shouldBe Some(r"7489051/5225670")
  }

  it should "define implement coefficients for FourOverPiLeibniz" in {
    val target: ConFrac = ConFrac(fPiBy4Leibniz)
    val z: LazyList[Pair] = target.coefficients
    z.head shouldBe Pair(1, 0)
    z.tail.head shouldBe Pair(2, 1)
  }

  behavior of "ConFrac.phi"

  it should "toDouble" in {
    ConFrac.phi.toDouble(1E-9, Hurwitz).get shouldBe goldenRatio +- 1E-4
  }

  import Ordering.Double.TotalOrdering

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // NOTE: we need a fairly large power because phi converges so slowly.
      val maybeDouble = ConFrac.phi.toDouble(epsilon, Hurwitz)
      maybeDouble match {
        case Some(x) => math.abs(x - goldenRatio) should be < epsilon: Unit
        case None => fail("unable to get Double from phi")
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ConFrac.e"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.E.take(20)
    val q: Rational = z.toRational
    q shouldBe r"410105312/150869313"
    q.toDouble shouldBe math.E +- 1E-10
  }

  behavior of "ConFrac.PiSimple"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.PiSimple
    val q: Rational = z.toRational
    q.toDouble shouldBe math.Pi +- 1E-10
  }

  behavior of "ConFrac.root2"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root2.take(20)
    val q: Rational = z.toRational
    q.toDouble shouldBe math.sqrt(2) +- 1E-10
  }

  it should "solve the Strand magazine puzzle" in {
    // NOTE: the solutions are the values of Y and X (numerator and denominator), e.g. 3, 2 or 17, 12
    // Now, the umber of houses on the street y is such that Y = 2y + 1
    // And the house number x is such that X = 2x
    // However, only the results from the odd numbers of terms are the solution to the Strand puzzle by Henry Dudeney,
    // which obeys the Pell equation = Y∧2 - 2 X∧2 = 1 (see https://en.wikipedia.org/wiki/Pell%27s_equation).
    // Incidentally, the even numbers of terms result in solutions to the complementary Pell equation: Y∧2 - 2 X∧2 = - 1
    // Obviously, if we solve Y∧2 - = 2 X∧2, we get the square root of 2, but this is an irrational number, not a rational number.
    val cf: ConFrac = ConFrac.root2
    cf.take(1).toRational shouldBe r"3/2" // NOTE: house # 1 on street of 1 houses
    cf.take(3).toRational shouldBe r"17/12" // NOTE: house # 6 on street of 8 houses
    cf.take(5).toRational shouldBe r"99/70" // NOTE: house # 35 on street of 49 houses
    cf.take(7).toRational shouldBe r"577/408" // NOTE: house # 204 on street of 288 houses
    cf.take(9).toRational shouldBe r"3363/2378" // NOTE: house # 1189 on street of 1681 houses
  }

  behavior of "ConFrac.root3"

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root3.take(20)
    val q: Rational = z.toRational
    q.toDouble shouldBe math.sqrt(3) +- 1E-10
  }

  behavior of "ConFrac.root19"

  val root19: Double = math.sqrt(19)

  it should "define ConFrac" in {
    val z: ConFrac = ConFrac.root19.take(20)
    val q: Rational = z.toRational
    q shouldBe r"512445947/117563163"
    (q * q).toDouble shouldBe 19.0 +- 1E-10
  }

  it should "toDouble" in {
    val maybeDouble = ConFrac.root19.toDouble(1E-6, Hurwitz)
    maybeDouble.nonEmpty shouldBe true
    // TODO: find out why we don't get precision of 1E-6
    maybeDouble.get shouldBe root19 +- 1.0E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // TODO: find out why we need this divisor of 6
      ConFrac.root19.toDouble(epsilon / 6, Hurwitz).foreach {
        x => math.abs(x - root19) should be < epsilon
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ConFrac.unapply"

  it should "work" in {
    val target: ConFrac = ConFrac.root19
    ConFrac.unapply(target) should matchPattern { case Some((4, Some(CF(1, _)))) => }
  }

  behavior of "ContinuedFraction.prefix"

  it should "create singleton ContinuedFraction" in {
    val target = ContinuedFraction.createInfinite(Pair(1)).prefix(0)
    target.cf.b shouldBe 1
    target.cf.tailOption shouldBe None
  }

  behavior of "ContinuedFraction.takeWhile"

  it should "phi ContinuedFraction precise to 1E-6" in {
    val target = ContinuedFraction.createInfinite(1)
    val epsilon = 1E-6

    def unprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = target.cf.takeWhile(unprecise)
    cf.reverseCoefficients.length shouldBe 16
    cf.toRational.toDouble shouldBe goldenRatio +- epsilon
  }

  behavior of "ContinuedFraction.toNominalRational"

  it should "implement toNominalRational" in {
    // NOTE: these tests approximate the Golden Ratio phi
    val target = ContinuedFraction.createInfinite(1)
    target.toRational(1) shouldBe Rational.two
    target.toRational(2) shouldBe r"3/2"
    target.toRational(3) shouldBe r"5/3"
    target.toRational(4) shouldBe r"8/5"
    target.toRational(5) shouldBe r"13/8"
    target.toRational(6) shouldBe r"21/13"
    target.toRational(7) shouldBe r"34/21"
    target.toRational(8) shouldBe r"55/34"
    target.toRational(9) shouldBe r"89/55"
    target.toRational(10) shouldBe r"144/89"
  }

  it should "implement toNominalRational(Double)" in {
    val cf: ContinuedFraction = ContinuedFraction.phi

    import Rational.RationalHelper
    cf.toRational(0.05) shouldBe Some(r"5/3")
    cf.toRational(0.01) shouldBe Some(r"13/8")
    cf.toRational(0.005) shouldBe Some(r"21/13")
    cf.toRational(0.002) shouldBe Some(r"34/21")
    cf.toRational(0.001) shouldBe Some(r"55/34")
    cf.toRational(0.0001) shouldBe Some(r"144/89")
  }

  behavior of "ContinuedFraction.phi"

  it should "toDouble" in {
    ContinuedFraction.phi.toDouble(1E-9).get shouldBe goldenRatio +- 1E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // NOTE: we need a fairly large power because phi converges so slowly.
      val maybeDouble = ContinuedFraction.phi.toDouble(epsilon)
      maybeDouble match {
        case Some(x) => math.abs(x - goldenRatio) should be < epsilon: Unit
        case None => fail("unable to get Double from phi")
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "ContinuedFraction.e"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.E
    val q: Rational = z.toRational(20)
    q shouldBe r"410105312/150869313"
    q.toDouble shouldBe math.E +- 1E-10
  }

  behavior of "ContinuedFraction.pi"

  it should "define ContinuedFraction.PiSimple" in {
    val z: ContinuedFraction = ContinuedFraction.PiSimple
    println(z.convergents.take(10).mkString("\n"))
    val qo = z.toRational(1E-10)
    qo should matchPattern { case Some(_) => }
    val ratio = qo.get
    println(s"ratio = $ratio")
    ratio.toDouble shouldBe math.Pi +- 1E-9
  }
  it should "get ContinuedFraction.PiSimple with precision 1 in 10^10" in {
    val z: ContinuedFraction = ContinuedFraction.PiSimple
    val qo = z.toRational(1E-10)
    qo should matchPattern { case Some(_) => }
    val ratio = qo.get
    println(s"ratio = $ratio")
    ratio.toDouble shouldBe math.Pi +- 1E-9
  }

  it should "define ContinuedFraction.PiSomayaji" in {
    val z: ContinuedFraction = ContinuedFraction.PiSomayaji
    // NOTE: this representation of pi converges somewhat slowly.
    val q: Rational = z.toRational(75)
    q.toDouble shouldBe math.Pi +- 1E-6
  }

  it should "define ContinuedFraction.PiA" in {
    val z: ContinuedFraction = ContinuedFraction.PiA
    val q: Rational = z.toRational(21)
    q.toDouble shouldBe math.Pi +- 1E-15
  }

  behavior of "ContinuedFraction.root2"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root2
    val q: Rational = z.toRational(20)
    q.toDouble shouldBe math.sqrt(2) +- 1E-10
  }

  behavior of "ContinuedFraction.root3"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root3
    val q: Rational = z.toRational(20)
    q.toDouble shouldBe math.sqrt(3) +- 1E-10
  }

  behavior of "ContinuedFraction.root19"

  it should "define ContinuedFraction" in {
    val z: ContinuedFraction = ContinuedFraction.root19
    val q: Rational = z.toRational(20)
    q shouldBe r"512445947/117563163"
    (q * q).toDouble shouldBe 19.0 +- 1E-10
  }

  it should "toDouble" in {
    val maybeDouble = ContinuedFraction.root19.toDouble(1E-6)
    maybeDouble.nonEmpty shouldBe true
    maybeDouble.get shouldBe root19 +- 1.0E-4
  }

  it should "implement toDouble(Double)" in {
    def checkValue(epsilon: Double): Unit = {
      // TODO: find out why we need this divisor of 6
      ContinuedFraction.root19.toDouble(epsilon / 6).foreach {
        x => math.abs(x - root19) should be < epsilon
      }
    }

    checkValue(0.1)
    checkValue(0.01)
    checkValue(0.001)
    checkValue(0.0001)
    checkValue(0.00001)
    checkValue(0.000001)
    checkValue(0.0000001)
    checkValue(0.00000001)
    checkValue(0.000000001)
  }

  behavior of "Worksheet"
  it should "" in {

    import com.phasmidsoftware.number.core.inner.Rational
    import com.phasmidsoftware.number.core.misc.ConFrac

    val ones = LazyList.continually(1L)
    val targetPhi = ConFrac.simple(ones)
    targetPhi.render(5) shouldBe "1 + 1/{1 + 1/{1 + 1/{1 + 1/{1 + 1/{...}}}}}"
    val epsilon = 1E-6

    def imprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

    val cf = targetPhi.takeWhile(imprecise)
    cf.toRational.toDouble shouldBe 1.618034447821682 +- epsilon

    targetPhi.renderConvergents(10)
    val conv = targetPhi.convergents.take(10).toList
    conv map (r => r.toDouble)

    val piSimple = ConFrac.PiSimple
    piSimple.renderConvergents(10) shouldBe "3, 22/7, 333/106, 355/113, 103993/33102, 104348/33215, 208341/66317, 312689/99532, 833719/265381, 1146408/364913, ..."

    val ramanujan2 = ConFrac.simple(LazyList.from(1) map (_.toLong) take 50)
    ramanujan2.renderConvergents(10) shouldBe "1, 3/2, 10/7, 43/30, 225/157, 1393/972, 9976/6961, 81201/56660, 740785/516901, 7489051/5225670, ..."
    ramanujan2.toRational shouldBe r"67958941000772916209065578406971589252705672441401340713037951251/47420026812410519916144115136972448359481892150574924818293966650"
  }
}
