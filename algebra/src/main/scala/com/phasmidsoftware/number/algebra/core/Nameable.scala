package com.phasmidsoftware.number.algebra.core

/**
  * A trait representing entities that can have an optional name.
  *
  * Classes or objects extending this trait provide a means to access an optional name,
  * which may or may not be present.
  */
trait Nameable {

  /**
    * Retrieves an optional name associated with the implementing class.
    *
    * The method returns an `Option` wrapping a `String`. If a name is available, it will be 
    * contained within the `Option` as `Some(name)`. If no name is present, the method will return `None`.
    *
    * @return an `Option[String]` representing the optional name
    */
  def maybeName: Option[String]

  /**
    * Determines if the name is protected, for example, it represents a mathematical symbol
    * for a transcendental value (e.g., pi), or a commonly used root such as √2.
    *
    * This method indicates whether a name associated with an entity is considered
    * protected. The protection status could influence how the name is accessed or
    * utilized by external systems.
    *
    * @return `true` if the name is protected, otherwise `false`
    */
  def protectedName: Boolean = false

  /**
    * Indicates whether symbolic names are preserved.
    *
    * This method determines if symbolic representation should be retained
    * when working with names in entities implementing the `Nameable` trait.
    *
    * @return `true` if symbolic names are kept, otherwise `false`
    */
  def keepSymbolic: Boolean = protectedName && maybeName.isDefined
}
