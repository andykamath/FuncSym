package SymMath

class Factorial(term: Operation) extends Operation {
  override def differentiate(wrt: SymVar): Operation = ???

  override def toString: String = term.toString + "!"
}
