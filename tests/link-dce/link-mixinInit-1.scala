import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 8, reachableMethods = 42)
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo

trait Foo {
  System.out.println(42)
}
