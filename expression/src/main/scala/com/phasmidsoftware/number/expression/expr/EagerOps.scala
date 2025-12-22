/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.{Eager, Valuable}

object EagerOps {
  extension (x: Eager) {
    def +(y: Eager): Valuable = Sum(x, y).normalize
    def *(y: Eager): Valuable = Product(x, y).normalize
    // etc.
  }
}