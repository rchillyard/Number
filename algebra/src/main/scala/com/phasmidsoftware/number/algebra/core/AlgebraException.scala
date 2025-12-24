/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * Represents an exception that occurs during algebraic computations or operations.
  *
  * @param message A descriptive message detailing the cause or context of the exception.
  * @param cause   The underlying throwable that caused this exception, if available.
  */
case class AlgebraException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
