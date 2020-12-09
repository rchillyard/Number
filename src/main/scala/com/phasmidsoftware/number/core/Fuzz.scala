package com.phasmidsoftware.number.core

//
///**
//  * Type constructor which will give fuzzy behavior to a type T.
//  * @tparam T
//  */
//trait Fuzz[T] {
//  val shape: Shape
//}
//
//trait RelativeFuzz[T] extends Fuzz[T] {
//  val tolerance: Double
////  def magnitude(t: T): T
//}
//
//trait AbsoluteFuzz[T] extends Fuzz[T] {
//  val magnitude: T
////  def tolerance(t: T): Double
//}
//
//trait Shape
//
//case object Box extends Shape
////case object Gaussian extends Shape
//
//object Fuzz {
//  implicit class Fuzzy[T](f: Fuzz[T]) {
//  }
//  implicit object MyFuzz extends Fuzz[Number]
//}