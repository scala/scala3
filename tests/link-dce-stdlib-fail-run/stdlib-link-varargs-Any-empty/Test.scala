import scala.annotation.internal

object Test {

  @internal.link.CallGraphBounds(reachableClasses = 350, classesWithReachableMethods = 300, reachableMethods = 550)
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
