import scala.annotation.experimental

@experimental // error
enum E:
  case A
  case B

def test: Unit =
  E.A // error
  E.B // error
  val e: E = ??? // error
  ()
