/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

case class AlgebraException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
