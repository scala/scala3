import language.experimental.captureChecking
import language.experimental.namedTypeArguments

def test2 =
  val x: Any^ = ???
  def foo[cap A, cap B >: {A}, T, U](x: Int) = 1
  foo[{x}, {x}, Int, String](0)
  foo[{}, {}, { def bar: Int }, { cap type D = {x} }](0)
  trait Foo { cap type D }
  foo[{}, {}, Foo, Foo](0)
  foo[cap A = {x}, cap B = {x}](0)
  foo[cap A = {x}](0)
  foo[T = Int](0)
  foo[T = Int, cap A = {x}](1)
  foo[cap A = {x}, T = Int](1)
  foo[cap B = {}, U = String, cap A = {}](1)