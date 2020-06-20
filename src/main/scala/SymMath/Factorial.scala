package SymMath

class Factorial(term: Operation) extends Operation {
  override def differentiate(wrt: SymVar): Operation = ???

  override def containsVariable(variable: SymVar): Boolean = this.term.containsVariable(variable)

  override def toString: String = term.toString + "!"

  override def equals(that: Any): Boolean = that match {
      case that: Factorial => that.equals(this.term)
      case _ => false
    }
}
