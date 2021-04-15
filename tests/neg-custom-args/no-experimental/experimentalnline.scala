import scala.annotation.experimental

@experimental // error
inline def g() = ()

def test: Unit =
  g() // errors
  ()
