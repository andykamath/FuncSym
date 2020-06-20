package SymMath

class Power(base: Operation, exponent: Operation) extends Operation {
  private def multiplyCoefficient(coefficient: Operation): Operation = {
    this.base * coefficient
  }

  override def +(that: Operation): Operation = {
    that match {
      case that: Power =>
        if(that.equals(this)) new Value(2) * this
         else super.+(that)
      case _ => super.+(that)
    }
  }

  private def hasSameBase(base: Operation): Boolean = base.equals(this.base)
  private def hasSameTerm(term: Operation): Boolean = term.equals(this.exponent)
  private def addExponent(thatExponent: Operation): Operation = this.exponent + thatExponent

  override def equals(that: Any): Boolean =
    that match {
      case that: Power => that.hasSameBase(this.base) && that.hasSameTerm(this.exponent)
      case _ => false
    }

  override def toString: String = {
    base match {
      case base: Value => exponent match {
        case exponent: Value => base.toString + " ^ " + exponent.toString
        case exponent: SymVar => base.toString + " ^ " + exponent.toString
        case _ => base.toString + " ^ (" + exponent.toString + ")"
      }
      case base: SymVar => exponent match {
        case exponent: Value => base.toString + " ^ " + exponent.toString
        case exponent: SymVar => base.toString + " ^ " + exponent.toString
        case _ => base.toString + " ^ (" + exponent.toString + ")"
      }
      case _ => exponent match {
        case exponent: Value => "(" + base.toString + ") ^ " + exponent.toString
        case exponent: SymVar => "(" + base.toString + ") ^ " + exponent.toString
        case _ => "(" + base.toString + ") ^ (" + exponent.toString + ")"
      }
    }
  }

  override def *(that: Operation): Operation = {
    that match {
      case that: Power =>
        if(that.hasSameTerm(this.exponent)) that.multiplyCoefficient(base) ^ exponent
        else if(that.hasSameBase(this.base)) this.base ^ that.addExponent(this.exponent)
        else super.*(that)
      case _ => super.*(that)
    }
  }

  override def /(that: Operation): Operation = {
    that match {
      case that: Power =>
        if(that.hasSameBase(this.base)) {
          this.base ^ new Value(-1) * that.addExponent(this.exponent * new Value(-1))
        }
        else super./(that)
      case _ => if(this.base.equals(that))
        this.base ^ this.exponent - new Value(1)
      else super./(that)
    }
  }

  override def differentiate(wrt: SymVar): Operation = {
    (base ^ exponent) * new Log(base) * exponent.differentiate(wrt)
  }
}
