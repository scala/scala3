//> using options -Yno-experimental

import scala.annotation.experimental

@experimental
inline def g() = ()

def test: Unit =
  g() // error
  ()
