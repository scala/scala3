import scala.annotation.experimental

@experimental // error
val x = ()

@experimental // error
def f() = ()

@experimental // error
object X:
  def fx() = 1

def test: Unit =
  f() // error
  x // error
  X.fx() // error
  import X.fx
  fx() // error
  ()
