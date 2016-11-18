
object Test {
  def main(args: Array[String]): Unit = {
    val a = new Foo
    a.foo()
  }
}

class Foo extends Bar {
  def foo(): Unit = super.bar()
}

trait Bar {
  def bar(): Unit = baz()
  def baz(): Unit = ()
}
