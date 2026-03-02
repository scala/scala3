sealed trait Foo
case object Bar extends Foo
case object Baz extends Foo

inline def id[A](a: A): A = a

def shouldThrowAWarning(foo: Foo) =
  id(foo match { // warn
    case Bar => "Bar"
  })
