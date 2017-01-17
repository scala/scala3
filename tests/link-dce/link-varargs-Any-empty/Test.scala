import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 172, classesWithReachableMethods = 53, reachableMethods = 215)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
