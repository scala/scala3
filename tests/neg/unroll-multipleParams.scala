//> using options -experimental

import scala.annotation.unroll

class Foo {
  final def foo(x: Int, @unroll y: Int = 0)(@unroll z: Int = 0) = x + y + z // error
}
