package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV2{
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(new Unrolled().foo(identity)("cow"), "cow1true")
    logAssertStartsWith(new Unrolled().foo(identity)("cow", 2), "cow2true")
    logAssertStartsWith(new Unrolled().foo(identity)("cow", 2, false), "cow2false")
  }
}
