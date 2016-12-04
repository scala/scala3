import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 123, classesWithReachableMethods = 13, reachableMethods = 62)
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo()

  def foo[U](ns: U*): Unit = {
    System.out.println(42)
  }

}
