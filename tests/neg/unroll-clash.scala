//> using options -experimental

import scala.annotation.unroll

class Foo {
  final def foo(x: Int, @unroll y: Int = 0) = x + y
  def foo(x: Int) = { // error
    println("Not binary compatible!")
    -1
  }
}

class UnrolledClass2(s: String) { // error

  def this(s: String, @unroll y: Boolean = true) = this(s + y)

  override def toString = s"UnrolledClass2($s)"
}
