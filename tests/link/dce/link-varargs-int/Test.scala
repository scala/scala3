import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 173, classesWithReachableMethods = 54, reachableMethods = 176)
  def main(args: Array[String]): Unit = {
    foo(1, 2, 3)
  }

  def foo(ns: Int*): Unit = {
    System.out.println(42)
  }
}
