//> using options -Werror

enum Foo {
  case Bar()
  case Qux()
}

case class Inv[A](a: A)

def tuple1(x: Tuple1[Foo]) = x match {
  case Tuple1[Foo.Bar @unchecked](_) => ()
  case Tuple1[Foo.Qux @unchecked](_) => ()
}

def inv(x: Inv[Foo]) = x match {
  case Inv[Foo.Bar @unchecked](_) => ()
  case Inv[Foo.Qux @unchecked](_) => ()
}
