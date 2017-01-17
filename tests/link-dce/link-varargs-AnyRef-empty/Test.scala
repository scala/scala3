import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 172, classesWithReachableMethods = 55, reachableMethods = 218)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: AnyRef*): Unit = {
    System.out.println(42)
  }
}
