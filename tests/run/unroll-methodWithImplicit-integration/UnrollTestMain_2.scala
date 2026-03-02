//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV2{
  def main(args: Array[String]): Unit = {
    implicit def f(s: String): String = s
    logAssertStartsWith(new Unrolled().foo("cow"), "cow1true")
    logAssertStartsWith(new Unrolled().foo("cow", 2), "cow2true")
    logAssertStartsWith(new Unrolled().foo("cow", 2, false), "cow2false")
  }
}
