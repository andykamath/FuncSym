package SymMath

class Coefficient(coefficient: Operation, term: Operation) extends Multiplication {
  private def addCoefficient(coefficient: Operation): Operation = {
    this.coefficient + coefficient
  }

  override def +(that: Operation): Operation = {
    that match {
      case that: Coefficient => new Coefficient(that.addCoefficient(coefficient), term)
      case _ => new Addition(this, that)
    }
  }

  private def hasSameCoefficient(coefficient: Operation): Boolean = coefficient.equals(this.coefficient)
  private def hasSameTerm(term: Operation): Boolean = term.equals(this.term)

  override def equals(that: Any): Boolean =
    that match {
      case that: Coefficient => that.hasSameCoefficient(this.coefficient) && that.hasSameTerm(this.term)
      case _ => false
    }

  override def toString: String = {
    coefficient match {
      case coefficient: Value => coefficient.toString + "(" + term.toString + ")"
      case _ => "(" + this.coefficient.toString + ")(" + term.toString + ")"
    }
  }
}
