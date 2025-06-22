sealed trait Adt
case class Foo() extends Adt
case class Bar() extends Adt {
  lazy val x: Nothing = throw new Exception()
}

inline def id[A](a: A): A = a

def shouldThrowAWarning(x: Adt) =
  id(x match { // warn
    case Foo() => "Bar"
  })
