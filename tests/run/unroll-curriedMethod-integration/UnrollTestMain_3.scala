//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV3{
  def main(args: Array[String]): Unit = {
    UnrollTestPlatformSpecificV3()
    
    logAssertStartsWith(new Unrolled().foo("cow")(identity), "cow1true0")
    logAssertStartsWith(new Unrolled().foo("cow", 2)(identity), "cow2true0")
    logAssertStartsWith(new Unrolled().foo("cow", 2, false)(identity), "cow2false0")
    logAssertStartsWith(new Unrolled().foo("cow", 2, false, 3)(identity), "cow2false3")
  }
}
