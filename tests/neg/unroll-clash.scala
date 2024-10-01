//> using options -experimental

import scala.annotation.unroll

class Foo {
  def foo(x: Int, @unroll y: Int = 0) = x + y
  def foo(x: Int) = { // error
    println("Not binary compatible!")
    -1
  }
}
