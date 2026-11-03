/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.CoreContext.AnyRoot
import com.phasmidsoftware.number.core.numerical.Constants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ContextSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Context"
  it should "or" in {
    val representation = NatLog
    val natLogContext = RestrictedContext(representation)
    natLogContext.representationQualifies(representation) shouldBe true
    ImpossibleContext.representationQualifies(representation) shouldBe false
    val contextOr = natLogContext `or` ImpossibleContext
    contextOr.representationQualifies(representation) shouldBe true
  }
  it should "and" in {
    val representation = NatLog
    val natLogContext = RestrictedContext(representation)
    natLogContext.representationQualifies(representation) shouldBe true
    ImpossibleContext.representationQualifies(representation) shouldBe false
    val contextOr = natLogContext `and` ImpossibleContext
    contextOr.representationQualifies(representation) shouldBe false
  }
  it should "qualifyingField" in {
    RestrictedContext(NatLog).qualifyingField(Some(Constants.e)) shouldBe Some(Constants.e)
  }
  it should "fieldQualifies" in {
    RestrictedContext(NatLog).fieldQualifies(Constants.e) shouldBe true
  }
  it should "not" in {
    val notLogContext = RestrictedContext(NatLog).not
    notLogContext.representationQualifies(NatLog) shouldBe false
    notLogContext.representationQualifies(PureNumber) shouldBe true
  }
  it should "AnyRoot" in {
    AnyRoot.representationQualifies(NatLog) shouldBe false
    AnyRoot.representationQualifies(SquareRoot) shouldBe true
    AnyRoot.representationQualifies(SquareRoot) shouldBe true
    AnyRoot.representationQualifies(PureNumber) shouldBe false
  }
  it should "AnyLog" in {

  }
  it should "AnyScalar" in {

  }
}
