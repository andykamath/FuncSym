package SymMath

class Power(base: Operation, exponent: Operation) extends Operation {
  def fractionForm: Operation = {
    if(this.exponent.isNegative) new Value(1) / (this.base ^ (this.exponent * new Value(-1)))
    else this
  }

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

  // TODO: Add support for implicit differentiation (x^x)
  override def differentiate(wrt: SymVar): Operation = {
    if(exponent.containsVariable(wrt) && base.containsVariable(wrt))
      throw new IllegalArgumentException("x can't be in base and power")
    else if(exponent.containsVariable(wrt)) (base ^ exponent) * new Log(base) * exponent.differentiate(wrt)
    else if(base.containsVariable(wrt)) exponent * (base ^ (exponent - new Value(1))) * base.differentiate(wrt)
    else throw new IllegalArgumentException("something wrong here")
  }

  override def containsVariable(variable: SymVar): Boolean = this.base.containsVariable(variable) ||
    this.exponent.containsVariable(variable)
}
