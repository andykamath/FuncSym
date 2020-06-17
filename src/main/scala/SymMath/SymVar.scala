package SymMath

class SymVar(v: String) extends ConstantTrait {
  override def +(that: Operation): Operation =
    that match {
      case v: SymVar => if(v.equals(this)) new Coefficient(new Value(2), this)
        else new Addition(this, v)
      case _ => new Addition(this, that)
    }


  override def toString: String = {
    this.v
  }

  private def hasSameString(s: String): Boolean = this.v.equals(s)

  override def equals(that: Any): Boolean =
    that match {
      case that: SymVar => that.hasSameString(this.v)
      case _ => false
    }
}
