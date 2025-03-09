import language.experimental.captureChecking
import caps.cap

trait Foo[T]
def test(): Unit =
  val a: Foo[Int]^{cap.rd} = ???
  val useA: () ->{a} Unit = ???
  def foo[X](x: Foo[X]^, op: () ->{x} Unit): Unit = ???
  foo(a, useA)
