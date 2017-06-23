import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 171, classesWithReachableMethods = 50, reachableMethods = 210)
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo()

  def foo[U](ns: U*): Unit = {
    System.out.println(42)
  }

}
