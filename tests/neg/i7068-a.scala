sealed trait Foo[T] {
  def foo(s: Foo[T]): Nothing
}

inline def species[T](t: T) = {
  case class FooT(x: T) extends Foo[T] { // error
    def foo(s: Foo[T]) = s match {
      case FooT(x) => ???
    }
  }
  FooT(t)
}

val foo = species(0)
