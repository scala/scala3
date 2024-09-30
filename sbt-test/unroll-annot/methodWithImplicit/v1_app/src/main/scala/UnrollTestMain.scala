package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestMainV1{
  def main(args: Array[String]): Unit = {
    implicit def f(s: String): String = s
    logAssertStartsWith(new Unrolled().foo("cow"), "cow")
  }
}
