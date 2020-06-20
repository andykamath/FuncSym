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
}
