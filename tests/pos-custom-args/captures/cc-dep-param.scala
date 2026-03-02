import language.experimental.captureChecking
import caps.any

trait Foo[T]
def test(): Unit =
  val a: Foo[Int]^{any.rd} = ???
  val useA: () ->{a} Unit = ???
  def foo[X](x: Foo[X]^, op: () ->{x} Unit): Unit = ???
  foo(a, useA)
