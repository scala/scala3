import scala.annotation.experimental

@experimental
enum E:
  case A
  case B

def test: Unit =
  E.A // error
  E.B // error
  val e: E = ??? // error
  ()
