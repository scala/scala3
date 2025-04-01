import language.experimental.captureChecking
import language.experimental.namedTypeArguments

def test2 =
  val x: Any^ = ???
  def foo[cap A, cap B >: {A}, T, U](x: Int) = 1
  //foo[{x}, {x}, Int](0) // TODO will not work for {x} in first arg
  foo[{}, {}, { def bar: Int }, { cap type D = {x} }](0)
  trait Foo { cap type D }
  foo[{}, {}, Foo, Foo](0)
  // foo[cap A = {x}, cap B = {x}](0)
  // foo[cap A = {x}](0)