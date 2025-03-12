import language.experimental.captureChecking
import language.experimental.namedTypeArguments

def test2 =
  val x: Any^ = ???
  def foo[cap A, B >: A](x: Int) = 1
  foo[cap x, x](0)
  foo[cap A = x, B = {x}](0)
  foo[cap A = {x}](0)