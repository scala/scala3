import language.experimental.captureChecking

trait Foo[T]
def test(): Unit =
  val a: Foo[Int]^ = ???
  val useA: () ->{a} Unit = ???
  def foo[X](x: Foo[X]^, op: () ->{x} Unit): Unit = ???
  foo(a, useA)
