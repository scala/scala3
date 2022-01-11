class Foo:
  erased given Int
  def foo(using erased x: Int): Unit = ()
  foo
