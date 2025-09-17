class Module {
  type BarTy
  sealed trait Adt[A]
  case class Foo() extends Adt[String]
  case class Bar[A <: BarTy](x: BarTy) extends Adt[A]
}

object Basic extends Module {
  type BarTy = String
}

def test(a: Basic.Adt[String]) = {
  a match { // warn: match may not be exhaustive
    case Basic.Foo() =>
  }
}

object Basic2 extends Module {
  type BarTy = Int
}

def test2(a: Basic2.Adt[String]) = {
  a match {
    case Basic2.Foo() =>
  }
}
