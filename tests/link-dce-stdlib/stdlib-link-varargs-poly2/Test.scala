import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 350, classesWithReachableMethods = 300, reachableMethods = 550)
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo(x)

  def foo[U](ns: U*): Unit = {
    System.out.println(42)
  }

}
