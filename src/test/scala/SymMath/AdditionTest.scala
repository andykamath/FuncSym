package SymMath
import math._
import org.scalatest._

class AdditionTest extends FlatSpec with Matchers {

  "A Value" should "return v1 + v2 as a Value of v1 + v2" in {
    val v1 = new Value(5)
    val v2 = new Value(10)
    assert((v1 + v2).equals(new Value(15)))
  }

  "A Variable (SymVar)" should "return x + y as an Addition when adding two separate variables" in {
    val x = new SymVar("x")
    val y = new SymVar("y")
    val z = new SymVar("z")
    val added = x + y

    val isSame: Boolean = added.equals(new Addition(x, y))

    val different = x + z
    val isNotSame: Boolean = !added.equals(different)

    assert(!x.equals(y))
    assert(isSame)
    assert(isNotSame)
  }

  "An Addition" should "be commutative" in {
    val x = new SymVar("x")
    val y = new SymVar("y")
    assert(new Addition(y, x).equals(new Addition(x, y)))
  }

  "Coefficients" should "add together and keep the terms the same" in {
    val addition = new Addition(new Value(8), new SymVar("x"))
    val c1 = new Coefficient(new Value(2), addition)
    val c2 = new Coefficient(new Value(3), addition)
    val tor = new Coefficient(new Value(5), addition)
    assert(tor.equals(c1 + c2))
  }
}
