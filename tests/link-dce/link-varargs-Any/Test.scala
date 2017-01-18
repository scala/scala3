import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 153, classesWithReachableMethods = 49, reachableMethods = 210)
  def main(args: Array[String]): Unit = {
    foo("a", "b", "c")
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
