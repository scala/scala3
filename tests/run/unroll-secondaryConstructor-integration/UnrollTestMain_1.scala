//> using options -experimental
package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV1{
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(new Unrolled("cow").foo, "cow1")
    logAssertStartsWith(new Unrolled("cow", 2).foo, "cow2")
  }
}
