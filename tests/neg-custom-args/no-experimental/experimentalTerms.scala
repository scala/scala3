import scala.annotation.experimental

@experimental // FIXME ERROR
val x = ()

@experimental // FIXME ERROR
def f() = ()

@experimental // FIXME ERROR
object X:
  def fx() = 1

def test: Unit =
  f() // error
  x // error
  X.fx() // error
  import X.fx
  fx() // error
  ()
