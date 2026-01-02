/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.Valuable
import com.phasmidsoftware.number.algebra.eager.Eager

object EagerOps {
  extension (x: Eager) {
    def +(y: Eager): Valuable = Sum(x, y).normalize
    def -(y: Eager): Valuable = BiFunction(Literal(x), -Literal(y), Sum).normalize
    def *(y: Eager): Valuable = Product(x, y).normalize
    def /(y: Eager): Valuable = BiFunction(Literal(x), Literal(y).reciprocal, Product).normalize
    // etc.
  }
}