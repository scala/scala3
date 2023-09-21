//> using options -language:experimental.erasedDefinitions

class Foo:
  erased given Int = 1
  def foo(using erased x: Int): Unit = ()
  foo
