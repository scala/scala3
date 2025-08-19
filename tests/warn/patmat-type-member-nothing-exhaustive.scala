trait Phase {
  type FooTy
  type BarTy
  sealed trait Adt
  case class Foo(x: FooTy) extends Adt
  case class Bar(x: BarTy) extends Adt
}

object Basic extends Phase {
  type FooTy = Unit
  type BarTy = Nothing
}


def test(a: Basic.Adt) = {
  a match
    case Basic.Foo(x) =>
}