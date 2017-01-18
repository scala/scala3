import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 154, classesWithReachableMethods = 50, reachableMethods = 210)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Int*): Unit = {
    System.out.println(42)
  }
}
