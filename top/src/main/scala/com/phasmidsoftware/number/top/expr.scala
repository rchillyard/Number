/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.expression.expr.ExpressionPrelude
import scala.language.implicitConversions

object expr:
  // DON'T export Predef at all - manually bring in only what we need
  export scala.Predef.{
    any2stringadd => _,  // Block this
    augmentString => _,   // Block this too
    _                     // Everything else
  }

  export ExpressionPrelude._
  export ExpressionPrelude.given
  export com.phasmidsoftware.number.parsenew.ExpressionParser._
  export com.phasmidsoftware.number.expression.expr.Empty._
  export com.phasmidsoftware.number.expression.expr.{∅, Expression}
  export com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps._

  // Explicitly provide a higher-priority + that forbids strings
  extension (e: Expression)
    def +(other: Int): Expression = e + ExpressionPrelude.given_Conversion_Int_Expression(other)
    def +(other: Double): Expression = e + ExpressionPrelude.given_Conversion_Double_Expression(other)
//import com.phasmidsoftware.number.expression.expr.ExpressionPrelude
//
//object expr:
//  export scala.Predef.{any2stringadd => _, _}
//  export ExpressionPrelude._
//  export ExpressionPrelude.given
//  export com.phasmidsoftware.number.parsenew.ExpressionParser._
//  export com.phasmidsoftware.number.expression.expr.Empty._
//  export com.phasmidsoftware.number.expression.expr.∅
//  export com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps._