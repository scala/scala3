import scala.language.`3.1-migration`

type X
def x: X = ???
def test: Unit =
  x match
    case y: X =>
