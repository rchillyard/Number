package com.phasmidsoftware.number.dimensions.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.compiletime.ops.int.*

class DimensionSpec extends AnyFlatSpec with should.Matchers {
  behavior of "Basic Dimensions"

  it should "verify Length * Length = Area" in {
    summon[MulDim[Length, Length] =:= Area]
  }

  it should "verify SqrtLength * SqrtLength = Length" in {
    summon[MulDim[SqrtLength, SqrtLength] =:= Length]
  }

  it should "verify Area / Length = Length" in {
    summon[DivDim[Area, Length] =:= Length]
  }

  it should "verify Length^2 = Area" in {
    summon[PowDim[Length, Two] =:= Area]
  }

  it should "verify Voltage * Time = MagneticFlux" in {
    summon[MulDim[Voltage, Time] =:= MagneticFlux]
  }

  it should "verify MagneticFlux / Area = MagneticFluxDensity" in {
    summon[DivDim[MagneticFlux, Area] =:= MagneticFluxDensity]
  }

  it should "verify MagneticFlux / Current = Inductance" in {
    summon[DivDim[MagneticFlux, Current] =:= Inductance]
  }

  it should "verify Inductance / Length = Permeability" in {
    summon[DivDim[Inductance, Length] =:= Permeability]
  }

  it should "verify Power * Time / Current = MagneticFlux" in {
    summon[DivDim[MulDim[Power, Time], Current] =:= MagneticFlux]
  }
}