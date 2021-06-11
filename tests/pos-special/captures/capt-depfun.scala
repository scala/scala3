class C
type Cap = C retains *
type Top = Any retains *

type T = (x: Cap) => String retains x.type

def f(y: Cap): String retains * =
  val a: T = (x: Cap) => ""
  val b = a(y)
  val c: String retains y.type = b
  c
