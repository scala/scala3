import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 173, classesWithReachableMethods = 54, reachableMethods = 215)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Int*): Unit = {
    System.out.println(42)
  }
}
