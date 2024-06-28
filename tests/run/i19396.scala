import scala.annotation.static

class Foo

object Foo {
  @static def foo = "foo"
}

class Bar {
  def bar = Foo.foo
}

object Test:
  def main(args: Array[String]): Unit =
    Foo.foo
    Bar().bar
