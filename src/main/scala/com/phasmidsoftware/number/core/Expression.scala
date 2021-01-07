package com.phasmidsoftware.number.core

trait Expression {

  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String

  /**
    * Eagerly compare this Expression with comparand.
    *
    * @param comparand the expression to be compared.
    * @return the result of comparing materialized this with materialized comparand.
    */
  def compare(comparand: Expression): Int = materialize.compare(comparand.materialize)
}

object Expression {

  implicit class ExpressionOps(x: Expression) {
    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Number): Expression = Sum(x, y)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Int): Expression = this.+(Number(y))

    /**
      * Method to lazily subtract the Number y from x.
      * TODO replace negate by lazy expression
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Number): Expression = Sum(x, -y)

    /**
      * Method to lazily subtract the Number y from x.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Int): Expression = this.-(Number(y))

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Number): Expression = Product(x, y)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Int): Expression = *(Number(y))

    /**
      * Method to lazily multiply the Number x by y.
      * TODO replace invert by lazy expression
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def /(y: Number): Expression = Product(x, y.invert)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def /(y: Int): Expression = /(Number(y))

    /**
      * Eagerly compare this expression with y.
      *
      * @param comparand the number to be compared.
      * @return the result of the comparison.
      */
    def compare(comparand: Number): Int = x compare comparand
  }

}

case class Sum(a: Expression, b: Expression) extends Expression {
  /**
    * Action to materialize this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = a.materialize add b.materialize

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.toString
}

case class Product(a: Expression, b: Expression) extends Expression {
  /**
    * Action to materialize this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = a.materialize multiply b.materialize

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.toString
}

case class Function(a: Expression, f: ExpressionFunction) extends Expression {
  /**
    * Action to materialize this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = f(a.materialize)

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.toString
}

case class ExpressionFunction(f: Number => Number, df: Number => Number, name: String) extends (Number => Number) {
  /**
    * Evaluate this function on x.
    *
    * @param x the parameter to the function.
    * @return the result of f(x).
    */
  override def apply(x: Number): Number = f(x)

  /**
    * Generate helpful debugging information about this ExpressionFunction.
    *
    * @return a String.
    */
  override def toString: String = s"function: $name"
}