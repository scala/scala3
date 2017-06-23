import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 7, reachableMethods = 9)
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo[Int](42)

trait Foo[T](n: Int) {
  System.out.println(n)
}
