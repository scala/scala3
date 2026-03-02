import language.experimental.captureChecking
import language.experimental.namedTypeArguments

def test2 =
  val x: Any^ = ???
  def foo[A^, B^ >: {A}, T, U](x: Int) = 1
  foo[{x}, {x}, Int, String](0)
  foo[{}, {}, { def bar: Int }, { type D^ = {x} }](0)
  trait Foo { type D^ }
  foo[{}, {}, Foo, Foo](0)
  foo[A = {x}, B = {x}](0)
  foo[A = {x}](0)
  foo[T = Int](0)
  foo[T = Int, A = {x}](1)
  foo[A = {x}, T = Int](1)
  foo[B = {}, U = String, A = {}](1)