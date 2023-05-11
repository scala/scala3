enum Foo[+F[_]]:
  case Bar[B[_]](value: Foo[B]) extends Foo[B]

class Test:
  def test[X[_]](foo: Foo[X]): Foo[X] = foo match
    case Foo.Bar(Foo.Bar(x)) => Foo.Bar(x)
    case _                   => foo
