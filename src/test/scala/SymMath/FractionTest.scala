package SymMath
import org.scalatest._

class FractionTest extends FlatSpec with Matchers {
  "Anything" should "be infinity when divided by zero" in {
    val inf = Constants.INF
    val zero = new Value(0)
    val const = new Value(5)
    assert(Constants.INF.equals(Constants.INF))
    assert(Constants.INF.equals(zero / zero))
    assert((inf / zero).equals(inf))
    assert((const / zero).equals(inf))
  }

  "Multiplications" should "be factored out" in {
    val x = new SymVar("x")
    val xplus5 = x + new Value(5)
    val xminus5 = x - new Value(5)
    assert((xplus5 / xplus5).equals(new Value(1)))
    assert(((xplus5 ^ new Value(3)) / xplus5).equals(xplus5 ^ new Value(2)))
    assert(((xplus5 ^ new Value(4)) / xplus5.^(new Value(2))).equals(xplus5.^(new Value(2))))
  }
}
