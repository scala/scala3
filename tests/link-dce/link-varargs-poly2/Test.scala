import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 173, classesWithReachableMethods = 54, reachableMethods = 215)
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo(x)

  def foo[U](ns: U*): Unit = {
    System.out.println(42)
  }

}
