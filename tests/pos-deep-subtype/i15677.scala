class Inv[A]

class Foo[B, M[_]]

class Bar[C, N[_]] extends Foo[C, N]:
  def inv: Inv[C] = ???

def test(foo: Foo[Int, Option]): Inv[Int] = foo match
  case bar: Bar[_, _] => bar.inv
  case _              => ???
