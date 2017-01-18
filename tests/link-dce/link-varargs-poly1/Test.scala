import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 153, classesWithReachableMethods = 49, reachableMethods = 210)
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo(x)

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }

}
