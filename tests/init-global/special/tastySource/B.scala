object B:
  var y = A.foo(bar) * 2

  def bar = C.n * 3      // warn

object C:
  var n = 10