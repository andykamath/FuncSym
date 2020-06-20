package SymMath

class SymVar(v: String) extends Operation {
  override def +(that: Operation): Operation =
    that match {
      case v: SymVar => if(v.equals(this)) new Value(2) * this
        else super.+(that)
      case _ => super.+(that)
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

  override def *(that: Operation): Operation = that match {
    case that: SymVar => if (that.equals(this)) super.^(new Value(2))  else super.*(that)
    case _ => super.*(that)
  }

  override def -(that: Operation): Operation = this + that * new Value(-1)

  override def /(that: Operation): Operation = that match {
    case that: Fraction => this * that.inverse
    case _ => super./(that)
  }

  override def differentiate(wrt: SymVar): Operation = if(!this.equals(wrt)) new Value(0) else new Value(1)
}
