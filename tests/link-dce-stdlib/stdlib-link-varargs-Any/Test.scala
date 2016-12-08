import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 311, classesWithReachableMethods = 271, reachableMethods = 511)
  def main(args: Array[String]): Unit = {
    foo("a", "b", "c")
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
