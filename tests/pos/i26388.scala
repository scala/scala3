//> using options -Werror

case class Foo[A](x: A)

def bar(x: Foo[[A] => A => A]): String =
  x match
    case Foo(f) => f("")
