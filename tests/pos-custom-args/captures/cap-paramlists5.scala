import language.experimental.captureChecking
import language.experimental.namedTypeArguments

def test2 =
  val x: Any^ = ???
  def foo[cap A, cap B >: {A}, T](x: Int) = 1
  foo[{x}, {x}, Int](0)
 // foo[cap A = {x}, cap B = {x}](0)
 // foo[cap A = {x}](0)