package unroll

import unroll.TestUtils.logAssertStartsWith


object UnrollTestMainV2{
  def main(args: Array[String]): Unit = {
    val unrolled = new UnrolledCls
    logAssertStartsWith(unrolled.foo("cow"), "cow1true".take(UnrollMisc.expectedLength))
    logAssertStartsWith(unrolled.foo("cow", 2), "cow2true".take(UnrollMisc.expectedLength))
    logAssertStartsWith(unrolled.foo("cow", 2, false), "cow2fals".take(UnrollMisc.expectedLength))

    logAssertStartsWith(UnrolledObj.foo("cow"), "cow1true".take(UnrollMisc.expectedLength))
    logAssertStartsWith(UnrolledObj.foo("cow", 2), "cow2true".take(UnrollMisc.expectedLength))
    logAssertStartsWith(UnrolledObj.foo("cow", 2, false), "cow2fals".take(UnrollMisc.expectedLength))
  }
}
