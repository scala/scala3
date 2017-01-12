import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 8, reachableMethods = 11)
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
