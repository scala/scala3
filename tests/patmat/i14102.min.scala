trait Foo[-X]
case class Bar(n: Int) extends Foo[Nothing]

def test[X](foo: Foo[X]) = foo match
  case Bar(1) =>
  case Bar(_) =>
  case _      =>
