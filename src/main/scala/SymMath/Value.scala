package SymMath

class Value(v: Int) extends Operation {
  override def factorial: Operation = if(this.equals(0) || this.equals(1)) new Value(1)
  else this * new Value(v - 1).factorial

  override def isNegative: Boolean = v < 0

  override def doOperation(): Operation = {
    this
  }

  override def toString: String = {
    this.v.toString
  }

  private def addValue(v: Int): Value = new Value(this.v + v)
  private def multiplyValue(v: Int): Value = new Value(this.v * v)

  override def +(that: Operation): Operation = {
    that match {
      case that: Value => that.addValue(this.v)
      case _ => super.+(that)
    }
  }

  override def *(that: Operation): Operation = that match {
    case that: Value => that.multiplyValue(this.v)
    case _ => super.*(that)
  }

  private def hasSameValue(v: Int): Boolean = this.v == v

  override def equals(that: Any): Boolean =
    that match {
      case that: Value => that.hasSameValue(this.v)
      case _ => false
    }

  override def differentiate(wrt: SymVar): Operation = new Value(0)
}
