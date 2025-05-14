/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.java

import com.phasmidsoftware.number.expression.Expression

object ExpressionJ {

  import Expression.ExpressionOps

  def add(x: Expression, y: Expression): Expression = x + y

  def multiply(x: Expression, y: Expression): Expression = x * y
}
