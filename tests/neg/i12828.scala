trait Foo[A]:
  def foo(x: A): Unit

trait Bar[A] extends Foo[A]:
  def foo(x: A & String): Unit = println(x.toUpperCase)

object Baz extends Bar[Int] // error: not implemented

@main def run() = Baz.foo(42)
