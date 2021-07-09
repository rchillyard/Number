package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogLevel, LogOff, MatchLogger, ~}
import com.phasmidsoftware.number.core.Field.convertToNumber
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  val sb = new StringBuilder
  implicit val logger: MatchLogger = SBLogger(LogOff, sb)

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    if (logger.logLevel != com.phasmidsoftware.matchers.LogOff)
      println("===============================\n")
  }

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit val em: ExpressionMatchers = new ExpressionMatchers {}
  private val two: Number = Number(2)
  private val one: Number = Number.one
  private val zero: Number = Number.zero
  private val half: Number = Number.two.invert


  behavior of "value"
  it should "work with value on Literal" in {
    val f: em.ExpressionMatcher[Field] = em.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = em.value
    f(one).successful shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = em.value
    f(FuzzyNumber(Right(1), Scalar, None)).successful shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: em.ExpressionMatcher[Field] = em.matchValue(one)
    val e = Literal(one)
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = em.matchValue(one)
    val g = f | em.matchValue(Number.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Number.pi)).successful shouldBe true
    g(Literal(Number.e)).successful shouldBe false
  }

  import com.phasmidsoftware.number.core.Expression.ExpressionOps

  behavior of "gathering operations"
  it should "gather 2 and * 1/2" in {
    val x: Expression = Number(7)
    val y = x.sqrt
    val z = y ^ 2
    val q = em.simplifier(z)
    q should matchPattern { case em.Match(_) => }
    q.get.simplify shouldBe Number(7)
  }

  behavior of "matchBiFunctionConstantResult"
  it should "match 1" in {
    val negativeOne: Number = Number(-1)
    val q = em.matchBiFunctionConstantResult(Product, negativeOne, zero)
    import em.TildeOps
    val r: em.MatchResult[Expression] = q(one ~ BiFunction(one, negativeOne, Product))
    r.successful shouldBe true
    r.get shouldBe zero
  }

  behavior of "matchEitherDyadic"
  it should "match (1, biFunction)" in {
    val negativeOne: Number = Number(-1)
    val p = em.matchEitherDyadic(commutes = true)
    import em.TildeOps
    val r: em.MatchResult[BiFunction ~ Expression] = p(one ~ BiFunction(one, negativeOne, Product))
    r.successful shouldBe true
  }
  it should "match (biFunction, 1)" in {
    val negativeOne: Number = Number(-1)
    val p = em.matchEitherDyadic(commutes = true)
    import em.TildeOps
    val r: em.MatchResult[BiFunction ~ Expression] = p(BiFunction(one, negativeOne, Product) ~ one)
    r.successful shouldBe true
  }

  behavior of "matchOffsetting"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = em.matchBiFunction & em.matchDyadicBranches(Sum) & em.matchOffsetting
    val result = p(z)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    val p = em.matchBiFunction & em.matchDyadicBranches(Sum) & em.matchOffsetting
    val result = p(z)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }

  behavior of "simplifier"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = em.simplifier
    val result = p(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    val p = em.simplifier
    val result = p(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = x + y
    val result = em.simplifier(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1 b" in {
    sb.append("cancel -1 and - 1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = y + x
    val result = em.simplifier(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe one
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe one
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z = y.simplify
    val q = z.simplify
    q shouldBe Number(7)
  }
  it should "show that lazy evaluation only works when you use it" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = x ^ 2
    y.materialize should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "show ^2 and sqrt for illustrative purposes" in {
    val seven = Number(7)
    val x = seven.sqrt
    val y: Expression = x ^ 2
    y should matchPattern { case BiFunction(FuzzyNumber(_, _, _), Literal(ExactNumber(Right(2), Scalar)), Power) => }
    convertToNumber(y.materialize) shouldEqual Number(7)
  }
  it should "cancel addition and subtraction" in {
    val x = one + 3 - 3
    val q = x.simplify
    q shouldBe one
  }
  it should "cancel multiplication and division" in {
    val x = Number.pi * 2 / 2
    val simplify = x.simplify
    simplify shouldBe Number.pi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Number.pi / 2 * 2
    val simplify = x.simplify
    simplify shouldBe Number.pi
  }

  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get shouldBe seven ^ one
  }
  it should "work for products" in {
    val q = new ExpressionMatchers().biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven * 2
    val y = x * 3
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Number(42)
  }
  it should "work for sums" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get shouldBe Number(12)
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (Number.e * 2).materialize.toString shouldBe "5.436563656918090(35)"
  }

  behavior of "matchSimplifyProductIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyProductIdentity
    import em.TildeOps
    p(Product ~ two ~ one) shouldBe em.Match(two)
    p(Product ~ one ~ two) shouldBe em.Match(two)
  }

  behavior of "matchSimplifyPlusIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifySumIdentity
    import em.TildeOps
    p(Sum ~ two ~ zero) shouldBe em.Match(two)
    p(Sum ~ zero ~ two) shouldBe em.Match(two)
  }

  behavior of "matchSimplifyPowerIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyPowerIdentity
    import em.TildeOps
    p(Power ~ two ~ one) shouldBe em.Match(two)
    p(Power ~ one ~ two) shouldBe em.Match(one)
  }

  behavior of "biFunctionSimplifier"
  it should "simplify" in {
    val p = em.biFunctionSimplifier
    p(BiFunction(two, zero, Sum)) shouldBe em.Match(two)
    p(BiFunction(zero, two, Sum)) shouldBe em.Match(two)
    p(BiFunction(two, one, Product)) shouldBe em.Match(two)
    p(BiFunction(one, two, Product)) shouldBe em.Match(two)
    p(BiFunction(two, one, Power)) shouldBe em.Match(two)
    p(BiFunction(one, two, Power)) shouldBe em.Match(one)
  }
  it should "evaluate (√3 + 1)(√3 - 1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 - 1)
    val q = x.simplify
    q.materialize shouldBe Number(2)
  }
  // NOTE: this will succeed only if we allow simplifications which reduce depth (but are not necessarily exact)
  // FIXME problem with distributePowerPower
  ignore should "simplify 1" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val z = p(x)
    z shouldBe em.Match(BiFunction(Number(3), Number(0.5), Power))
  }

  behavior of "matchBiFunctionConstantResult"
  it should "simplify" in {
    val p: em.Matcher[em.Expressions, Expression] = em.matchBiFunctionConstantResult(Product, Number(-1), zero)
    import em.TildeOps
    p(BiFunction(two, Number(-1), Product) ~ two) shouldBe em.Match(zero)
    p(two ~ BiFunction(two, Number(-1), Product)) shouldBe em.Match(zero)
    p(BiFunction(Number(-1), two, Product) ~ two) shouldBe em.Match(zero)
    p(two ~ BiFunction(Number(-1), two, Product)) shouldBe em.Match(zero)
  }

  behavior of "simplifyProduct"
  it should "work for root(3) * root(3)" in {
    val x = Expression(3).sqrt
    val z = em.simplifyProduct(x, x)
    z.isExact shouldBe true
  }

  behavior of "biFunctionSimplifier"
  it should "simplify 1 + 2 - 2" in {
    val p = em.biFunctionSimplifier
    val x = one plus two - two
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe one
  }
  it should "properly simplify 1 + 2 - 2 + 0" in {
    val p = em.biFunctionSimplifier
    val x = one plus two - two + zero
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe one
  }
  behavior of "matchSimplifySum"
  it should "properly simplify 1 + root3 - root3 + 3" in {
    val p = em.matchSimplifySum
    val z: Expression = Number(3).sqrt
    val x = z plus -z + zero
    import em.TildeOps
    val r = p(Sum ~ one ~ x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe one
  }

  behavior of "gatherSum"
  it should "work" in {
    val p: em.Matcher[em.Expressions, Expression] = em.gatherSum
    val z: Expression = Number(3).sqrt
    val x = z plus -z + zero
    import em.TildeOps
    val r = p(one ~ x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe one
  }
  behavior of "distributor"
  it should "distribute" in {
    val p = em.distributor
    import em.TildeOps
    val a = BiFunction(one, two, Sum)
    val b = BiFunction(two, one, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Number(9))
  }
  it should "distributeProductSum" in {
    val p = em.distributeProductSum
    val a = BiFunction(two, Number(3), Sum)
    val b = BiFunction(Number(4), two, Sum)
    import em.TildeOps
    val z = p(a ~ b)
    z shouldBe em.Match(Number(30))
  }
  it should "distributeProductSum a" in {
    val p = em.distributeProductSum
    val a = BiFunction(one, two, Sum)
    val b = BiFunction(two, one, Sum)
    import em.TildeOps
    val z = p(a ~ b)
    z shouldBe em.Match(Number(9))
  }
  it should "distributeProductSum b" in {
    val p = em.distributeProductSum
    val x = Number("2.00")
    val y = Number("3.00")
    val a = BiFunction(one, x, Sum)
    val b = BiFunction(half, y, Sum)
    import em.TildeOps
    val z = p(a ~ b)
    z shouldBe em.Match(Number(10.5))
  }
  // NOTE This expression won't simplify because it is inexact (and distribution doesn't reduce the depth).
  ignore should "distributeProductSum c" in {
    val p = em.distributeProductSum
    val x = Number("2.00*")
    val y = Number("3.00*")
    val a = BiFunction(one, x, Sum)
    val b = BiFunction(half, y, Sum)
    import em.TildeOps
    val z = p(a ~ b)
    val eo = Expression.parse("( ( 3.00* + 0.5 ) + ( 2.00* * ( 3.00* + 0.5 ) ) )")
    eo map (z shouldBe em.Match(_)) orElse fail("could not parse expression")
  }
  it should "distributeProductSum d" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val y = -x
    val a = BiFunction(one, x, Sum)
    val b = BiFunction(one, y, Sum)
    val r = BiFunction(a, b, Product)
    val z: em.MatchResult[Expression] = p(r)
    val k: em.MatchResult[Expression] = z map (_.simplify)
    k shouldBe em.Match(Number(-2))
  }
  it should "distributePowerProduct" in {
    val p = em.distributePowerProduct
    val x = Expression(3).sqrt
    val a = BiFunction(two, x, Product)
    import em.TildeOps
    val z = p(a ~ two)
    z shouldBe em.Match(Number(12))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    val p = em.distributeProductPower
    val x = Expression(3).sqrt
    import em.TildeOps
    val z = x ~ x
    val q: em.MatchResult[Expression] = p(z)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Number(3)
  }

  behavior of "simplify"
  it should "work" in {
    val xo = Expression.parse("( 3 ^ ( 2 ^ -1 ) )")
    val yo = Expression.parse("( ( 3 ^ ( 2 ^ -1 ) ) * -1 )")
    val zo = for (x <- xo; y <- yo) yield em.simplifyProduct(x, y)
    zo should matchPattern { case Some(_) => }
    zo.get shouldBe BiFunction(Number(3), Number(-1), Product)
  }
}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () })

