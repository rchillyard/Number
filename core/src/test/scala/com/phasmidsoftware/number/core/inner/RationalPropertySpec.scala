/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties
import org.scalatest.Assertions.fail

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by scalaprof on 10/4/16.
  *
  * NOTE: the Double property constrains its domain to inputs whose reciprocal is finite. It used
  * to fail intermittently for a subnormal x, where 1.0 / x overflows (#601); the advice then was
  * to re-run, which is what kept the underlying defect in createExact alive.
  */
class RationalPropertySpec extends Properties("Rational") {

  import com.phasmidsoftware.number.core.inner.Rational.r

  property("FromString") = forAll { (a: Int, b: Short) =>
    val r = r"$a/$b"
    Rational.hasCorrectRatio(r, a, b.toLong)
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

  property("Double") = forAll { (x: Double) =>
    import org.scalactic.Tolerance.*
    import org.scalactic.TripleEquals.*
    // NOTE the property is about a round trip through the reciprocal, so it only means anything
    // where the reciprocal exists as a Double. For a subnormal x, 1.0 / x overflows to Infinity,
    // and there is no Rational to compare against -- which used to surface as a NumberFormatException
    // from createExact and an intermittent build failure (#601).
    (x != 0.0 && (1.0 / x).isFinite) ==> {
      // CONSIDER check this is OK. Might need to be Rational(BigDecimal.valueOf(x)) (#377)
      val ry = Rational.createExact(x)
      val sy = Rational.createExact(1.0 / x)
      (for (r <- ry; s <- sy) yield r * s) match {
        case Success(z) => z.toDouble === 1.0 +- 1E-6
        case Failure(x) => fail(s"exception: $x")
      }
    }
  }

  property("Division") = forAll { (a: Short, b: Short) =>
    try Rational(a.toLong, b.toLong).render != ""
    catch {
      case NonFatal(x) => throw new Exception(s"in Rational($a,$b) caused by ${x.getLocalizedMessage}")
    }
  }
}