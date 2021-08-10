package com.phasmidsoftware.number.core

trait NumberLike {

    /**
      * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
      *
      * @param maybeFactor the (optional) context in which we want to evaluate this Expression.
      *                    if factor is None then, the result will depend solely on whether this is exact.
      * @return true if this NumberLike object is exact in the context of factor, else false.
      */
    def isExact(maybeFactor: Option[Factor]): Boolean

    /**
      * Method to determine if this Field is actually a real Number (i.e. not complex).
      * NOTE: to force this as a Number, use convertToNumber in the companion Object.
      *
      * @return a Some(x) if this is a Number; otherwise return None.
      */
    def asNumber: Option[Number]

    /**
      * Method to render this Field in a presentable manner.
      *
      * @return a String
      */
    def render: String
}
