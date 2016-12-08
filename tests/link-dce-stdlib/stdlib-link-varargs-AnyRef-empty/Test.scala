import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 304, classesWithReachableMethods = 263, reachableMethods = 496)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: AnyRef*): Unit = {
    System.out.println(42)
  }
}
