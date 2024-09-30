//> using options -experimental
package example
// !! IMPORTANT: If you remove this test, also remove unroll-caseclass.check

import scala.annotation.unroll

// v2 of Unrolled
case class Unrolled(s: String, n: Int = 1, @unroll b: Boolean = true){
  def foo = s + n + b
}

// v2: ammendments to code that exercise a new parameter
object UnrollTestMainV2 extends TestUtil {
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(new Unrolled("cow").foo, "cow1true")
    logAssertStartsWith(new Unrolled("cow", 2).foo, "cow2true")
    logAssertStartsWith(new Unrolled("cow", 2, false).foo, "cow2false")

    logAssertStartsWith(Unrolled("cow").foo, "cow1true")
    logAssertStartsWith(Unrolled("cow", 2).foo, "cow2true")
    logAssertStartsWith(Unrolled("cow", 2, false).foo, "cow2false")

    val unrolled = Unrolled("cow")

    logAssertStartsWith(unrolled.copy(s = "cow").foo, "cow1true")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2).foo, "cow2true")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2, b = false).foo, "cow2false")

    val Unrolled(s, n, b) = unrolled

    assert(s == "cow")
    assert(n == 1)
    assert(b == true)

    UnrollTestScalaSpecificV2.test()
  }
}

object UnrollTestScalaSpecificV2 extends TestUtil {
  def test() = {
    val unrolled = summon[scala.deriving.Mirror.Of[Unrolled]].fromProduct(
      new Product {
        def canEqual(that: Any) = true
        def productArity = 3
        def productElement(n: Int) = n match {
          case 0 => "hello"
          case 1 => 31337
          case 2 => false
        }
      }

    )
    logAssertStartsWith(unrolled.foo, "hello31337false")
  }
}
