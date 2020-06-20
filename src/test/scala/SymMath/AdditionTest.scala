package SymMath
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

  "Exponents" should "always return an addition unless equal" in {
    val addition = new Addition(new Value(8), new SymVar("x"))
    val multiplication = new Multiplication(new Value(8), new SymVar("x"))
    val p1 = new Value(2) ^ addition
    val p_diff = new Value(3) ^ multiplication
    val tor = new Value(2) * p1
    assert(tor.equals(p1 + p1))
    assert(!tor.equals(p1 + p_diff))
  }

  "Pre-defined Constants" should "add together" in {
    val e = Constants.E
    val pi = Constants.PI
    val two_e = new Multiplication(new Value(2), Constants.E)
    val e_plus_pi = new Addition(e, pi)
    assert(two_e.equals(e + e))
    assert(e_plus_pi.equals(e + pi))
  }

  "Logarithms" should "multiply when the bases are same and add otherwise" in {
    val val5 = new Value(5)
    val varX = new SymVar("x")
    val varZ = new SymVar("z")
    val val10 = new Value(10)

    val log1 = val5.log()
    val log2 = varX.log()
    val unnatural_log = varZ.log(val10)

    val same_base = new Log(val5 * varX)
    val different_base = new Addition(log1, unnatural_log)
    assert(same_base.equals(log1 + log2))
    assert(different_base.equals(log1 + unnatural_log))
  }

  "Subtraction" should "be the negative of addition" in {
    val x = new SymVar("x")
    val y = new SymVar("y")
    val const1 = new Value(5)
    val const2 = new Value(6)

    assert((x - y).equals(new Addition(x, new Multiplication(new Value(-1), y))))
    assert((const2 - const1).equals(new Value(1)))
  }

  "Logs" should "multiply when added with the same base" in {
    val val5 = new Value(5)
    val val10 = new Value(10)
    val log1 = new Log(val5)
    val log2 = new Log(val10)
    val base_10 = new Log(val5, val10)

    assert(log1 + log2 == new Log(val5 * val10))
    assert(log1 + base_10 == new Addition(log1, base_10))
  }
}
