import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 10, classesWithReachableMethods = 4, reachableMethods = 8)
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
