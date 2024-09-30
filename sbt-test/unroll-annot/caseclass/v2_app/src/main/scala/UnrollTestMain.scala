package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV2{
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
