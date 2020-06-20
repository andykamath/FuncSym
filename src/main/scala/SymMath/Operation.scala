package SymMath

trait Operation {
  def doOperation(): Operation = this
  def isNegative: Boolean = false
  def +(that: Operation): Operation = {
    if(that.equals(new Value(0))) this
    else if(this.equals(new Value(0))) that
    else if(this.equals(Constants.INF) || that.equals(Constants.INF)) Constants.INF
    else new Addition(this, that)
  }
  def *(that: Operation): Operation = {
    if(that.equals(new Value(1))) this
    else if(this.equals(new Value(1))) that
    else if(this.equals(Constants.INF) || that.equals(Constants.INF)) Constants.INF
    else if(that.equals(new Value(0))) new Value(0)
    else new Multiplication(this, that)
  }
  def -(that: Operation): Operation = this + that * new Value(-1)
  def /(that: Operation): Operation = that match {
    case that: Fraction => that.inverse * this
    case _ =>
      if(that.equals(new Value(1))) this
      else if(that.equals(Constants.INF)) {
        if(this.equals(Constants.INF) || this.equals(new Value(0))) Constants.INF
        else new Value(0)
      }
      else if(that.equals(new Value(0))) Constants.INF
      else if(this.equals(new Value(0))) new Value(0)
      else if(this.equals(that)) new Value(1)
      else new Fraction(this, that)
  }
  def ^(that: Operation): Operation =
    if(this.equals(Constants.INF)) Constants.INF
    else if(that.equals(Constants.INF)) Constants.INF
    else if(that.equals(new Value(0))) new Value(1)
    else if(that.equals(new Value(1))) this
    else new Power(this, that)
  def log(base: Operation=Constants.E): Operation = {
    if(base.equals(this)) new Value(1)
    else new Log(this, base)
  }
  def toString: String
  def equals(that: Any): Boolean
  def differentiate(wrt: SymVar): Operation
  def factorial: Operation = new Factorial(this)
  def containsVariable(variable: SymVar): Boolean = false
}
