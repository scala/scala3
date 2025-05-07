//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV3{
  def main(args: Array[String]): Unit = {
    UnrollTestPlatformSpecificV3()
    
    implicit def f(s: String): String = s
    logAssertStartsWith(new Unrolled().foo("cow"), "cow1true0")
    logAssertStartsWith(new Unrolled().foo("cow", 2), "cow2true0")
    logAssertStartsWith(new Unrolled().foo("cow", 2, false), "cow2false0")
    logAssertStartsWith(new Unrolled().foo("cow", 2, false, 3), "cow2false3")
  }
}
