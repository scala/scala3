import language.experimental.captureChecking

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val bar = [C^, D <: {C}, E^ <: {C,x}, F >: {x,y} <: {C,E}] => (x: Int) => 1