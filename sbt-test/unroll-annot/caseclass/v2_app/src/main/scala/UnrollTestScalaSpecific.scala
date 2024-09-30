package unroll

import unroll.TestUtils.logAssertStartsWith

object UnrollTestScalaSpecificV2{
  def test() = {
    val unrolled = Unrolled.fromProduct(
      new Product {
        def canEqual(that: Any) = true
        def productArity = 3
        def productElement(n: Int) = n match {
          case 0 => "hello"
          case 1 => 31337
          case 2 => false
        }
      }

    )
    logAssertStartsWith(unrolled.foo, "hello31337false")
  }
}