class Inv[A]

class Foo[B, M[_]]

class Bar[C, N[_]] extends Foo[C, N]:
  def inv: Inv[C] = null

def test(foo: Foo[Int, Option]): Inv[Int] = foo match
  case bar: Bar[?, ?] => bar.inv
  case _              => null
