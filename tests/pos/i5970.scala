//> using options -Werror -deprecation -feature

object Test {
  case class Foo[T](t: T)

  def foo[T](ft: Unit|Foo[T]): T = {
    ft match {
      case Foo(t) => t
      case () => ???
    }
  }

  foo(Foo(23))
}
