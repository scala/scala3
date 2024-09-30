package unroll
import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV1{
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
