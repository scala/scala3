package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV2{
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(Unrolled.foo("cow"), "cow1true")
    logAssertStartsWith(Unrolled.foo("cow", 2), "cow2true")
    logAssertStartsWith(Unrolled.foo("cow", 2, false), "cow2false")
  }
}
