package com.phasmidsoftware.number.java

import com.phasmidsoftware.number.core.Expression

object ExpressionJ {

  import com.phasmidsoftware.number.core.Expression.ExpressionOps

  def add(x: Expression, y: Expression): Expression = x + y

  def multiply(x: Expression, y: Expression): Expression = x * y
}
