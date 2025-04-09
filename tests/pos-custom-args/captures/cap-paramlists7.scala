import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val baz2 = (i: Int) => [cap C, cap D <: {C}, cap E <: {C,x}, cap F >: {x,y} <: {C,E} : Ctx] => (x: Int) => 1