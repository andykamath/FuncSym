package SymMath

class Multiplication(val symbols: Operation*) extends Operation {
  override def doOperation(): Operation = ???

  private def hasSameTerms(symbols: Operation*): Boolean = {
    val thoseSymbols = symbols.groupBy(identity).view.mapValues(_.size).toMap
    val theseSymbols = symbols.groupBy(identity).view.mapValues(_.size).toMap
    thoseSymbols.equals(theseSymbols)
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Multiplication => that.hasSameTerms(this.symbols : _*)
      case _ => false
    }
}
