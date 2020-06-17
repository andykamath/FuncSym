package SymMath

trait Operation {
  type T
  def doOperation(): Operation = this
  def +(that: Operation): Operation = new Addition(this, that)
  def toString: String
  def equals(a: Any): Boolean
}
