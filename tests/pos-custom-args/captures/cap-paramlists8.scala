import language.experimental.captureChecking

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  val baz3 = (i: Int) => [C^, D^ <: {C}, E^ <: {C,x}] => () => [F^ >: {x,y} <: {C,E}] => (x: Int) => 1