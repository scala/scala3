import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val baz2 = (i: Int) => [cap C, D <: C, E <: {C,x}, F >: {x,y} <: {C,E} : Ctx] => (x: Int) => 1