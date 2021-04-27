import annotation.targetName

class Alpha[T]:

  def foo() = 1

  @targetName("bar") def foo(x: T): T = x

  @targetName("baz") def foo(x: String): String = x

class Beta extends Alpha[String]: // error: Name clash between defined and inherited member: foo/baz

  def bar(x: String): String = x  // OK, since no bridge is generated

  def baz(x: String): String = x