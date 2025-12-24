/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

import com.phasmidsoftware.number.algebra.*

/**
  * The `TypeSafe` trait provides mechanisms for querying the runtime type and 
  * hierarchical category of an implementing class. It is designed to enhance debugging 
  * and logging by offering detailed descriptive information about the implementing 
  * instance's type and category within its type hierarchy.
  */
trait TypeSafe {

  /**
    * Returns a simple string representation of the runtime type.
    * Useful for debugging and logging.
    *
    * @return the simple class name (e.g., "WholeNumber", "RationalNumber")
    */
  def typeName: String = getClass.getSimpleName

  /**
    * Returns the category of this value in the Valuable type hierarchy.
    *
    * @return a string indicating the high-level category (e.g., "Expression", "Structure", "Nat", "Complex")
    */
  def category: String = this match {
    case _: Lazy => "Lazy"
    case _: Structure => "Structure"
    case _: Nat => "Nat"
    case _: Complex => "Complex"
    case _: Eager => "Eager"
    case _ => "Valuable"
  }

  /**
    * Returns a descriptive string combining category and type name.
    * Useful for debugging to understand both the hierarchy level and concrete type.
    *
    * @return a string in the format "Category.TypeName" (e.g., "Structure.WholeNumber")
    */
  def describe: String = s"$category.$typeName"
}
