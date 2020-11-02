import annotation.targetName


abstract class Alpha[T] {

  def foo() = 1

  @targetName("bar") def foo(x: T): T

  @targetName("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

class Beta extends Alpha[String] {

  @targetName("foo1") override def foo() = 1    // error

  @targetName("baz") def foo(x: String): String = x ++ x   // error

  override def ++ (xs: Alpha[String]): Alpha[String] = this   // error

}
