import annotation.targetName


abstract class Alpha[T] {

  def foo() = 1

  @targetName("bar") def foo(x: T): T

  @targetName("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

class Beta extends Alpha[String] {  // error: needs to be abstract

  @targetName("foo1") override def foo() = 1    // error: different signature than overridden

  @targetName("baz") def foo(x: String): String = x ++ x   // OK,

  override def ++ (xs: Alpha[String]): Alpha[String] = this   // error: different signature than overidden

}
