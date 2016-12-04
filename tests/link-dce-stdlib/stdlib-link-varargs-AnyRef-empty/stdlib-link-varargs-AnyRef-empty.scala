import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 123, classesWithReachableMethods = 13, reachableMethods = 62)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: AnyRef*): Unit = {
    System.out.println(42)
  }
}
