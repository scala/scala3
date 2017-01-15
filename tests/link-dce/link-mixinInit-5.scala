import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 24, classesWithReachableMethods = 9, reachableMethods = 45)
  def main(args: Array[String]): Unit = {
    new Foo
  }
}

class Foo extends Bar {
  def foo(): Unit = System.out.println(42)
}

trait Bar extends Baz

trait Baz {
  foo()
  def foo(): Unit
}
