import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 9, reachableMethods = 43)
  def main(args: Array[String]): Unit = {
    new Bar
  }
}


class Bar extends Foo

trait Foo {
  val foo: Int = {
    System.out.println(43)
    43
  }
}
