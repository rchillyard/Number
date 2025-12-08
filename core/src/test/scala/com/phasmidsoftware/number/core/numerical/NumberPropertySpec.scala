package com.phasmidsoftware.number.core.numerical

import org.scalacheck.Properties

/**
  * Created by scalaprof on 10/29/22.
  */
class NumberPropertySpec extends Properties("Number") {

//  property("NumberFromString") = forAll { (a: Int, b: Int) =>
//    val wholeString = s"$a"
//    val decimalString = if (b > 0) s".$b" else if (b == 0) "" else s".${math.abs(b)}"
//    val string = wholeString ++ decimalString
//    println(string)
//    val n = Number(string)
//    println(n)
//    n.isExact shouldBe true
//    val mo: Option[Number] = n.multiply(Number(10).power(decimalString.length - 1)).asNumber
//    println(mo)
//    val zo = for (m <- mo; x <- m.toInt) yield x
//    println(zo)
//      zo.isDefined
//  }

}