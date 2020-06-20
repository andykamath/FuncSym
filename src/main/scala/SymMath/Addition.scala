package SymMath

import scala.collection.immutable.::

class Addition(val symbols: Operation*) extends Operation {
  override def doOperation(): Operation = {
    symbols.reduce((x: Operation, y: Operation) => x + y)
  }

  override def toString: String = {
    this.symbols.map(x => x.toString).mkString(" + ")
  }

  private def hasSameTerms(symbols: Operation*): Boolean = {
    val thoseSymbols = symbols.groupBy(identity).view.mapValues(_.size).toMap
    val theseSymbols = this.symbols.groupBy(identity).view.mapValues(_.size).toMap
    thoseSymbols.equals(theseSymbols)
  }

  override def equals(that: Any): Boolean = that match {
      case that: Addition => that.hasSameTerms(this.symbols : _*)
      case _ => false
    }

  private def mergeTerms(symbols: Operation*): Operation =
    symbols.map(x => this.addTerm(x, this.symbols)).reduce((x, y) => x + y)

  @scala.annotation.tailrec
  private def addTerm(v: Operation, symbols: Seq[Operation]): Operation = {
    val hd::tail = symbols
    hd match {
      case hd: Value => hd + v + tail.reduce((x, y) => x + y)
      case _ => addTerm(v, tail)
    }
  }

  override def +(that: Operation): Operation =
    that match {
      case that: Addition => that.mergeTerms(this.symbols: _*)
      case _ => new Addition(List(that) ++ this.symbols: _*)
    }

  override def differentiate(wrt: SymVar): Operation = symbols.map(_.differentiate(wrt)).reduce((x, y) => x + y)

  override def containsVariable(variable: SymVar): Boolean = this.symbols.exists(_.containsVariable(variable))
}
