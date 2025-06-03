//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV2{
  def main(args: Array[String]): Unit = {
    val unrolled = new Unrolled{}
    logAssertStartsWith(unrolled.foo("cow"), "cow1true")
    logAssertStartsWith(unrolled.foo("cow", 2), "cow2true")
    logAssertStartsWith(unrolled.foo("cow", 2, false), "cow2false")

    logAssertStartsWith(Unrolled.foo("cow"), "cow1true")
    logAssertStartsWith(Unrolled.foo("cow", 2), "cow2true")
    logAssertStartsWith(Unrolled.foo("cow", 2, false), "cow2false")
  }
}
