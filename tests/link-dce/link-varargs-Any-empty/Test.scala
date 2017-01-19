import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 171, classesWithReachableMethods = 50, reachableMethods = 211)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
