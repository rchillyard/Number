package com.phasmidsoftware.number.model

/**
  * @author scalaprof
  */
trait Valuable[X] {
  def get: X
}
