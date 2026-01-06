/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.expression.expr.{Expression, ExpressionPrelude}
import scala.language.implicitConversions

/**
  * Aggregator object providing convenient imports for working with Expressions.
  *
  * Usage:
  * {{{
  *   import expr._
  *
  *   val result = ∅ + 1 + 2 * 3
  *   val expr = math"x^2 + 3x - 5"
  * }}}
  */
object expr:
  // Block string concatenation implicits while importing everything else from Predef
  export scala.Predef.{any2stringadd => _, augmentString => _, _}

  // Expression construction and constants
  export ExpressionPrelude._
  export ExpressionPrelude.given

  // LaTeX parsing
  export com.phasmidsoftware.number.parse.ExpressionParser._

  // Empty operator and related utilities
  export com.phasmidsoftware.number.expression.expr.Empty._
  export com.phasmidsoftware.number.expression.expr.{∅, Expression}

  // LaTeX rendering
  export com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps._

  // Explicit operators to ensure Expression arithmetic takes precedence over string concatenation
  extension (e: Expression)
    def +(other: Int): Expression =
      e + ExpressionPrelude.given_Conversion_Int_Expression(other)

    def +(other: Double): Expression =
      e + ExpressionPrelude.given_Conversion_Double_Expression(other)