package unroll

import unroll.TestUtils.logAssertStartsWith



object UnrollTestMainV1{
  def main(args: Array[String]): Unit = {
    val unrolled = new UnrolledCls
    logAssertStartsWith(unrolled.foo("cow"), "cow1")
    logAssertStartsWith(unrolled.foo("cow", 2), "cow2")

    logAssertStartsWith(UnrolledObj.foo("cow"), "cow1")
    logAssertStartsWith(UnrolledObj.foo("cow", 2), "cow2")
  }
}
