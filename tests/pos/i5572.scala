trait Foo {
  type Id[t] = t
  transparent inline def foo[T](t: T): Id[T] =
    inline t match {
      case i: Int => (i+1).asInstanceOf[Id[T]]
      case _ => t
    }
}

object Bar extends Foo

object Test {
  Bar.foo(23)
}
