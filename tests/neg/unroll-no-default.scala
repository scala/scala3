//> using options -experimental -Xprint:unrollDefs

import scala.annotation.unroll

class Foo {
  def foo(x: Int, @unroll y: Int) = x + y // error
}
