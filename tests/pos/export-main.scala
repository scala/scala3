object Foo:
  @main def baz: Int = 1

object Bar:
  export Foo.baz // export Foo.baz but not create an new main entry point
