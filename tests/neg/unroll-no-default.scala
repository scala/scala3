//> using options -experimental -Vprint:unrollDefs

import scala.annotation.unroll

class Foo {
  final def foo(x: Int, @unroll y: Int) = x + y // error
}
