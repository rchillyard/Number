package com.phasmidsoftware.number.dimensions.core

/**
  * A custom exception class that represents errors related to dimension mismatches
  * or invalid dimensions in a specific context (e.g., matrix or array operations).
  *
  * @param message The error message describing the nature of the dimension issue.
  */
case class DimensionsException(message: String) extends Exception(message)
