import scala.annotation.experimental

@experimental
val x = ()

@experimental
def f() = ()

@experimental
object X:
  def fx() = 1

def test1: Unit =
  f() // error: def f is marked @experimental and therefore ...
  x // error: value x is marked @experimental and therefore ...
  X.fx() // error: object X is marked @experimental and therefore ...
  import X.fx
  fx() // error: object X is marked @experimental and therefore ...

@experimental
def test2: Unit =
  // references to f, x and X are ok because `test2` is experimental
  f()
  x
  X.fx()
  import X.fx
  fx()
