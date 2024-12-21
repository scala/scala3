//> using options -experimental
// scalajs: --skip
package example
// !! IMPORTANT: If you remove this test, also remove unroll-caseclass.check

// import scala.annotation.unroll <- v1 did not need to unroll yet

// v1 of Unrolled
case class Unrolled(s: String, n: Int = 1) {
  def foo = s + n
}

// v1: original code that compiled against v1 of Unrolled
object UnrollTestMainV1 extends TestUtil {
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(new Unrolled("cow").foo, "cow1")
    logAssertStartsWith(new Unrolled("cow", 2).foo, "cow2")

    logAssertStartsWith(Unrolled("cow").foo, "cow1")
    logAssertStartsWith(Unrolled("cow", 2).foo, "cow2")

    val unrolled = Unrolled("cow")

    logAssertStartsWith(unrolled.copy(s = "cow").foo, "cow1")
    logAssertStartsWith(unrolled.copy(s = "cow", n = 2).foo, "cow2")

    val Unrolled(s, n) = unrolled

    assert(s == "cow")
    assert(n == 1)

    UnrollTestScalaSpecificV1.test()
  }
}

object UnrollTestScalaSpecificV1 extends TestUtil {
  def test() = {
    val unrolled = summon[scala.deriving.Mirror.Of[Unrolled]].fromProduct(
      new Product{
        def canEqual(that: Any) = true
        def productArity = 2
        def productElement(n: Int) = n match{
          case 0 => "hello"
          case 1 => 31337
        }
      }
    )

    logAssertStartsWith(unrolled.foo, "hello31337")
  }
}

trait TestUtil {
  def logAssertStartsWith(actual: String, expected: String): Unit = {
    assert(actual.startsWith(expected))
    val suffix = {
      val suffix0 = actual.stripPrefix(expected)
      if (suffix0.isEmpty) "" else s""" + "$suffix0""""
    }
    println(s"""Assertion passed: found "$expected"$suffix""")
  }
}
