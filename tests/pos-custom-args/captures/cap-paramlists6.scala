import language.experimental.captureChecking

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val baz  = () => [C^, D^ <: {C}, E^ <: {C,x}, F^ >: {x,y} <: {C,E}] => (x: Int) => 1