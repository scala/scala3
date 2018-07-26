
object Test {
  def main(args: Array[String]): Unit = {
    Foo.foo
  }
}

object Foo extends Bar {
  transparent def foo: Unit = bar
}

class Bar {
  def bar: Unit = println("bar")
}
