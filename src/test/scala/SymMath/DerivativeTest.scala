package SymMath
import org.scalatest._

class DerivativeTest extends FlatSpec with Matchers {
  "A Value" should "differentiate as 0" in {
    val v1 = new Value(5)
    assert(v1.differentiate(new SymVar("x")).equals(new Value(0)))
  }

  "Constants" should "differentiate as 0 unless infinity" in {
    val wrt = new SymVar("x")
    assert(Constants.INF.differentiate(wrt).equals(Constants.INF))
    assert(Constants.PI.differentiate(wrt).equals(new Value(0)))
    assert(Constants.E.differentiate(wrt).equals(new Value(0)))
  }

  "SymVars" should "differentiate to 1 if they are being differentiated" in {
    val wrt = new SymVar("x")
    val y = new SymVar("y")
    assert(y.differentiate(wrt).equals(new Value(0)))
    assert(wrt.differentiate(wrt).equals(new Value(1)))
  }

  "Multiplication" should "follow the chain rule" in {
    val x = new SymVar("x")
    val y = new SymVar("y")
    assert((x * y).differentiate(x).equals(new SymVar("y")))
  }

  "Division" should "follow the quotient rule" in {
    val x = new SymVar("x")
    val num = x ^ new Value(2)
  }
}
