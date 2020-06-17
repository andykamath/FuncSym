package SymMath

class Value(v: Int) extends ConstantTrait {
  override def doOperation(): Operation = {
    this
  }

  override def toString: String = {
    this.v.toString
  }

  private def addValue(v: Int): Value = new Value(this.v + v)

  override def +(that: Operation): Operation = {
    that match {
      case that: Value => that.addValue(this.v)
      case _ => new Addition(this, that)
    }
  }

  private def hasSameValue(v: Int): Boolean = this.v == v

  override def equals(that: Any): Boolean =
    that match {
      case that: Value => that.hasSameValue(this.v)
      case _ => false
    }
}
