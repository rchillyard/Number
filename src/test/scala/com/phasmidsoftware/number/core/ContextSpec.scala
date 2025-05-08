/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Context.AnyRoot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ContextSpec extends AnyFlatSpec {

  behavior of "Context"

  it should "or" in {
    val factor = NatLog
    val natLogContext = RestrictedContext(factor)
    natLogContext.factorQualifies(factor) shouldBe true
    ImpossibleContext.factorQualifies(factor) shouldBe false
    val contextOr = natLogContext or ImpossibleContext
    contextOr.factorQualifies(factor) shouldBe true
  }

  it should "and" in {
    val factor = NatLog
    val natLogContext = RestrictedContext(factor)
    natLogContext.factorQualifies(factor) shouldBe true
    ImpossibleContext.factorQualifies(factor) shouldBe false
    val contextOr = natLogContext and ImpossibleContext
    contextOr.factorQualifies(factor) shouldBe false
  }

  it should "qualifyingField" in {
    RestrictedContext(NatLog).qualifyingField(Some(Constants.e)) shouldBe Some(Constants.e)
  }

  it should "fieldQualifies" in {
    RestrictedContext(NatLog).fieldQualifies(Constants.e) shouldBe true
  }

  it should "not" in {
    val notLogContext = RestrictedContext(NatLog).not
    notLogContext.factorQualifies(NatLog) shouldBe false
    notLogContext.factorQualifies(PureNumber) shouldBe true
  }

  it should "AnyRoot" in {
    AnyRoot.factorQualifies(NatLog) shouldBe false
    AnyRoot.factorQualifies(Root2) shouldBe true
    AnyRoot.factorQualifies(Root2) shouldBe true
    AnyRoot.factorQualifies(PureNumber) shouldBe false
  }

  it should "AnyLog" in {

  }

  it should "AnyScalar" in {

  }

}
