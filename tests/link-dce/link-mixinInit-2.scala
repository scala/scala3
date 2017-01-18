import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 8, reachableMethods = 43)
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo(42)

trait Foo(n: Int) {
  System.out.println(n)
}
