class C
type Cap = C retains *
type Top = Any retains *

type T = (x: Cap) => List[String retains x.type] => Unit // error
val x: (x: Cap) => Array[String retains x.type] = ??? // error
val y = x

def test: Unit =
  def f(x: Cap) = // error
    val g = (xs: List[String retains x.type]) => ()
    g
  ()
