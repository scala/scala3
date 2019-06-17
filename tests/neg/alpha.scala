import annotation.alpha


abstract class Alpha[T] {

  def foo() = 1

  @alpha("bar") def foo(x: T): T

  @alpha("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

class Beta extends Alpha[String] {

  @alpha("foo1") override def foo() = 1    // error

  @alpha("baz") def foo(x: String): String = x ++ x   // error

  override def ++ (xs: Alpha[String]): Alpha[String] = this   // error

}
