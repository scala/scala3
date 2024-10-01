//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV3{
  def main(args: Array[String]): Unit = {
    UnrollTestPlatformSpecificV3()
    
    logAssertStartsWith(new Unrolled("cow").foo, "cow1true0")
    logAssertStartsWith(new Unrolled("cow", 2).foo, "cow2true0")
    logAssertStartsWith(new Unrolled("cow", 2, false).foo, "cow2false0")
    logAssertStartsWith(new Unrolled("cow", 2, false, 3).foo, "cow2false3")
  }
}
