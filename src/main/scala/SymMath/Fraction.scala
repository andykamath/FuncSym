package SymMath

class Fraction(numerator: Operation, denominator: Operation) extends Operation {
  // TODO: ADD GCD FUNCTION
  private def changeNumerator(that: Operation) = that * this.denominator
  private def multiplyDenominator(that: Operation) = that * this.denominator
  private def multiplyNumerator(that: Operation) = that * this.numerator
  private def hasSameNumerator(that: Operation) = this.numerator.equals(that)
  private def hasSameDenominator(that: Operation) = this.denominator.equals(that)

  def inverse: Operation = denominator / numerator

  override def +(that: Operation): Operation = {
    that match {
      case that: Fraction =>
        val that_numerator = that * this.denominator
        val this_numerator = that.changeNumerator(this.numerator)
        val common_denominator = that.multiplyDenominator(this.denominator)
        (that_numerator + this_numerator) / common_denominator
    }
  }

  override def *(that: Operation): Operation = that match {
    case that: Fraction =>
      that.multiplyNumerator(this.numerator) / that.multiplyDenominator(this.denominator)
    case _ => (that / this.denominator) * this.numerator
  }

  override def -(that: Operation): Operation = this + that * new Value(-1)

  override def /(that: Operation): Operation = that match {
    case that: Fraction => this * that.inverse
    case _ =>
      if(that.equals(new Value(0))) Constants.INF
      else this.numerator / (this.denominator * that)
  }

  override def differentiate(wrt: SymVar): Operation = {
    val new_numerator = numerator.differentiate(wrt) * denominator - numerator * denominator.differentiate(wrt)
    val new_denominator = denominator ^ new Value(2)
    new_numerator / new_denominator
  }

  override def toString: String = "(" + this.numerator.toString + ")" + " / " + "(" + this.denominator.toString + ")"
  override def containsVariable(variable: SymVar): Boolean = this.numerator.containsVariable(variable) ||
    this.denominator.containsVariable(variable)

  override def equals(that: Any): Boolean = {
    val tor = that match {
      case that: Fraction => that.hasSameDenominator(this.denominator) && that.hasSameNumerator(this.numerator)
      case that: Power => that.fractionForm.equals(this)
      case _ => false
    }
    tor
  }
}
