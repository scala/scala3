object Foo {
  private final val x = 1
  private def y = 2
}

class Foo {
  import Foo._
  inline def foo = x + Foo.x + y + Foo.y
}

class Test {
  (new Foo).foo
}
