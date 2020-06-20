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

  override def equals(that: Any): Boolean =
    that match {
      case that: Addition => that.hasSameTerms(this.symbols : _*)
      case _ => false
    }

  private def mergeTerms(symbols: Operation*): Addition = new Addition(this.symbols ++ symbols : _*)

  override def +(that: Operation): Operation =
    that match {
      case that: Addition => that.mergeTerms(this.symbols: _*)
      case _ => new Addition(List(that) ++ this.symbols: _*)
    }

  override def differentiate(wrt: SymVar): Operation = new Addition(symbols.map(_.differentiate(wrt)) :_*)
}
