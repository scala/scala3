@annotation.capability class Cap

def test(x: Cap) =

  def foo(y: Cap) = if x == y then println()

  val x1 = foo

  val x2 = identity(x1)

  val x3: Cap -> Unit = x2 // error