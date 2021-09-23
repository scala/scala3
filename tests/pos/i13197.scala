// this test is similar to explicit-nulls/pos/i13197.scala, but without explicit nulls

extension [T](x: T | String) inline def forceString: x.type & String =
  x.asInstanceOf

trait Bar:
  def b: String | Int

class Foo(a: String = "", b: String)

object Foo:
  def foo(bar: Bar) = Foo(b = bar.b.forceString)