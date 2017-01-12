import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo[Int]

trait Foo[T] {
  System.out.println(42)
}
