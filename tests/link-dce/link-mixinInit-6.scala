import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 9, reachableMethods = 12)
  def main(args: Array[String]): Unit = {
    new Foo
  }
}

class Foo extends Foo2 with Bar {
  def foo(): Unit = System.out.println(42)
}

abstract class Foo2 extends Baz

trait Bar extends Baz

trait Baz {
  foo()
  def foo(): Unit
}
