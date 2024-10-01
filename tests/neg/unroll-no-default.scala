//> using options -experimental

import scala.annotation.unroll

class Foo {
  def foo(x: Int, @unroll y: Int) = x + y // error
}
