package p

abstract class Foo {
  type T
  def f(x: T): List[T] = List()
}
object Bar extends Foo { type T = String }
