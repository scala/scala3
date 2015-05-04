package test

trait X { def foo(x: Int): Int; def bar = foo(2) }
trait XX extends X

object test {
  val x: X = (x: Int) => 2  // should be a closure
  val xx: XX = (x: Int) => 2  // should be a closure, but blows up in backend
}
