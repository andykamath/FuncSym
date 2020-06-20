package SymMath

class Log(term: Operation, base: Operation = Constants.E) extends Operation {
  private def isSameBase(base: Operation) = base.equals(this.base)
  private def isSameTerm(term: Operation) = term.equals(this.term)
  private def multiplyTerms(term: Operation) = term * this.term

  override def equals(that: Any): Boolean =
    that match {
      case that: Log => that.isSameBase(this.base) && that.isSameTerm(this.term)
      case _ => false
    }

  override def +(that: Operation): Operation = {
    that match {
      case that: Log =>
        if(that.isSameBase(this.base)) new Log(that.multiplyTerms(this.term), this.base)
          else super.+(that)
      case _ => super.+(that)
    }
  }

  override def toString: String = "LOG("+term.toString+" BASE "+base.toString+")"

  override def /(that: Operation): Operation = that match {
    case that: Fraction => this * that.inverse
    case _ => super./(that)
  }

  override def differentiate(wrt: SymVar): Operation = {
    term.differentiate(wrt)  / (new Log(term) * wrt) // Chain rule
  }
}
