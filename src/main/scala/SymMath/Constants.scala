package SymMath

sealed case class Constant(rep: String, value: Number) extends Operation {
  override def +(that: Operation): Operation =
    that match {
      case v: Constant => if(v.equals(this)) new Value(2) * this
      else super.+(that)
      case _ => super.+(that)
    }

  private def isSameValue(s: String): Boolean = this.rep.equals(s)

  override def equals(that: Any): Boolean = that match {
      case that: Constant => that.isSameValue(this.rep)
      case _ => false
    }

  override def toString: String = rep

  override def differentiate(wrt: SymVar): Operation = if(this.equals(Constants.INF)) Constants.INF else new Value(0)
}

object Constants extends Enumeration {
  val E: Constant = Constant("E", 2.71828)
  val PI: Constant = Constant("PI", 3.14159)
  val INF: Constant = Constant("INF", -1)
}

