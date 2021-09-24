trait Bar:
  def b: String | Null

class Foo(a: String = "", b: String)

object Foo:
  def foo(bar: Bar) = Foo(b = bar.b.nn)