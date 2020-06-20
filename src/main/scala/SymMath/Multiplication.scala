package SymMath

class Multiplication(val symbols: Operation*) extends Operation {
  override def isNegative: Boolean = symbols.count {
    _.isNegative
  } % 2 != 0

  private def hasSameTerms(symbols: Operation*): Boolean = {
    val thoseSymbols = symbols.groupBy(identity).view.mapValues(_.size).toMap
    val theseSymbols = symbols.groupBy(identity).view.mapValues(_.size).toMap
    thoseSymbols.equals(theseSymbols)
  }

  private def mergeTerms(symbols: Operation*): Multiplication = new Multiplication(this.symbols ++ symbols: _*)

  private def factorOut(symbols: Operation*): Operation = {
    var newOp: Operation = new Multiplication(symbols:_*)
    this.symbols.foreach(thisSym => {
      newOp = newOp / thisSym
    })
    newOp
  }

  private def divide(symbol: Operation, symbols: Seq[Operation]): Operation = {
    val hd::tail = symbols
    hd / symbol match {
      case div: Fraction => hd * divide(symbol, tail)
      case _ => tail.reduce((x, y) => x * y)
    }
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Multiplication => that.hasSameTerms(this.symbols: _*)
      case _ => false
    }

  override def toString: String = {
    val values = symbols.filter(x => {
      x match {
        case _: Value => true
        case _: SymVar => true
        case _ => false
      }
    })
    val valString: String = values.map(_.toString).mkString(" * ")
    val ops = symbols.filter(x => !values.contains(x))
    if (ops.isEmpty) valString
    else "(" + valString + ")" + ops.map(x => "(" + x.toString + ")").mkString("")
  }

  // TODO: Fix Addition for factoring

  override def *(that: Operation): Operation =
    that match {
      case that: Multiplication => that.mergeTerms(this.symbols: _*)
      case that: Fraction => that * this
      case _ => if (that.equals(new Value(1))) this
      else new Multiplication(this.symbols ++ List(that): _*)
    }

  override def /(that: Operation): Operation = that match {
    case that: Multiplication => that.factorOut(this)
    case _ => this.divide(that, this.symbols)
  }

  override def differentiate(wrt: SymVar): Operation = {
    symbols.zipWithIndex.map({ case (sym, i) =>
      var syms: Operation = new Value(1)
      symbols.zipWithIndex.foreach { case (notSame, diffIndex) =>
        if (i == diffIndex) syms *= sym.differentiate(wrt)
        else syms *= notSame
      }
      syms
    }).reduce((x, y) => x + y)
  }

  override def containsVariable(variable: SymVar): Boolean = this.symbols.exists(_.containsVariable(variable))
}
