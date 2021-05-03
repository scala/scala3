import scala.annotation.experimental

@experimental // FIXME ERROR
inline def g() = ()

def test: Unit =
  g() // errors
  ()
