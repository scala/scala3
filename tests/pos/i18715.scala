case class Foo(x: Int = 0)

extension (x: Any)
  private def foo = Foo
  export foo.apply
