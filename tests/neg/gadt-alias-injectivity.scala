object Test {
  enum EQ[A, B] {
    case Refl[T]() extends EQ[T, T]
  }
  import EQ._

  object A {
    type Foo[+X] = (X, X)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x
    }
  }

  object B {
    type Foo[X] = (X, X)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x
    }
  }

  object C {
    type Foo[+X] = Int | (X, X)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x
    }
  }

  object D {
    type Foo[+X] = (Int, Int)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x // error
    }
  }

  trait E {
    type Foo[+X] <: Int | (X, X)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x // error
    }
  }

  trait F {
    type Foo[X] >: Int | (X, X)
    def foo[X, Y](x: X, eq: EQ[Foo[X], Foo[Y]]): Y = eq match {
      case Refl() => x // error
    }
  }
}
