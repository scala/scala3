import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 24, classesWithReachableMethods = 9, reachableMethods = 44)
  def main(args: Array[String]): Unit = {
    new Foo().foo
  }
}

class Foo extends Bar

trait Bar extends Baz {
  override def foo = System.out.println(42)
}

trait Baz {
  @internal.link.AssertNotReachable
  def foo = System.out.println(5)
}
