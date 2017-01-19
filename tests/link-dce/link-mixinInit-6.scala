import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 25, classesWithReachableMethods = 8, reachableMethods = 11)
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
