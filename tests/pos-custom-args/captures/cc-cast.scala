import annotation.unchecked.uncheckedCaptures
import compiletime.uninitialized

def foo(x: Int => Int) = ()


object Test:
  def test(x: Object) =
    foo(x.asInstanceOf[Int => Int])

  @uncheckedCaptures var x1: Object^ = uninitialized
  @uncheckedCaptures var x2: Object^ = _
