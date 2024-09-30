package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV1{
  def main(args: Array[String]): Unit = {
    logAssertStartsWith(new Unrolled().foo("cow")(identity), "cow")
  }
}
