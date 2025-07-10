sealed trait Adt
case class Foo() extends Adt
case class Bar() extends Adt {
  lazy val x: Nothing = throw new Exception()
}

def shouldThrowAWarning(x: Adt) =
  x match { // warn
    case Foo() => "Foo"
  }
