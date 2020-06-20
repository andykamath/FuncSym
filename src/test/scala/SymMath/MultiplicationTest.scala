package SymMath
import org.scalatest._

class MultiplicationTest extends FlatSpec with Matchers {
  "Infinity" should "always be infinity in any multiplication" in {
    val inf = Constants.INF
    val zero = new Value(0)
    val const = new Value(5)
    assert((zero * inf).equals(inf))
    assert((inf * inf).equals(inf))
    assert((const * inf).equals(inf))
  }

  "Does factoring work?" should "not work" in {
    val x = new SymVar("x")
    println(((x + new Value(2)) ^ new Value(2)))
    assert(((x + new Value(2)) ^ new Value(2)).equals((x ^ new Value(2)) + new Value(2) * x + new Value(4)))
  }
}
