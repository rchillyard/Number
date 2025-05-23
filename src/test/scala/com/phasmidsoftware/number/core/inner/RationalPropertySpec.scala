/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalatest.Assertions.fail
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by scalaprof on 10/4/16.
  *
  * NOTE: it is possible that these tests will fail due to randomly generating a Double precision number out of range of Double (E311).
  * Recommend just re-run.
  */
class RationalPropertySpec extends Properties("Rational") {

  import com.phasmidsoftware.number.core.inner.Rational.RationalHelper

  property("FromString") = forAll { (a: Int, b: Short) =>
    val r = r"$a/$b"
    Rational.hasCorrectRatio(r, a, b)
  }

  property("FromIntAndShort") = forAll { (a: Int, b: Short) =>
    val _a: BigInt = BigInt(a) * 1000
    val r = Rational(_a, b)
    Rational.hasCorrectRatio(r, _a, b.toLong)
  }

  property("Addition") = forAll { (a: Long, b: Short, c: Int, d: Short) =>
    val r1 = Rational(a, b)
    val r2 = Rational(c, d)
    val r = r1 + r2
    //      println(s"$a/$b, $c/$d => $r1 + $r2 = $r")
    try Rational.hasCorrectRatio(r, (BigInt(a) * d.toInt) + (BigInt(c) * b.toInt), b.toLong * d)
    catch {
      case NonFatal(x) => throw new Exception(s"a=$a, b=$b, c=$c, d=$d => $r1 + $r2 = $r (${r.n}/${r.d}) caused by ${x.getLocalizedMessage}")
    }
  }

  property("Double") = forAll { x: Double =>
    import org.scalactic.Tolerance._
    import org.scalactic.TripleEquals._
    // CONSIDER check this is OK. Might need to be Rational(BigDecimal.valueOf(x))
    val ry = Rational.createExact(x)
    val sy = Rational.createExact(1.0 / x)
    (for (r <- ry; s <- sy) yield r * s) match {
      case Success(z) => z.toDouble === 1.0 +- 1E-6
      case Failure(x) => fail(s"exception: $x")
    }

  }

  property("Division") = forAll { (a: Short, b: Short) =>
    try Rational(a, b).render != ""
    catch {
      case NonFatal(x) => throw new Exception(s"${Rational(a, b)} caused by ${x.getLocalizedMessage}")
    }
  }
}