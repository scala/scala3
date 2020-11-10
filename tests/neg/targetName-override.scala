import annotation.targetName


abstract class Alpha[T] {

  def foo() = 1

  @targetName("foo1") def foo(x: T): T

  @targetName("append") def ++ (xs: Alpha[T]): Alpha[T] = this

}

class Beta extends Alpha[String] {  // error: needs to be abstract

  @targetName("foo1") override def foo() = 1    // error: should not have a target name

  @targetName("baz") def foo(x: String): String = x ++ x   // error: has a different target name annotation

  override def ++ (xs: Alpha[String]): Alpha[String] = this   // error: misses a targetname annotation

}
