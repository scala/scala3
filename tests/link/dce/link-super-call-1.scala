import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Foo
    a.foo()
  }
}

class Foo extends Bar {
  @internal.link.AssertReachable def foo(): Unit = super.bar()
}

trait Bar {
  @internal.link.AssertReachable def bar(): Unit = baz()
  @internal.link.AssertReachable def baz(): Unit = ()
}
