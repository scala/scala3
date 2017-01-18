import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 153, classesWithReachableMethods = 50, reachableMethods = 211)
  def main(args: Array[String]): Unit = {
    foo(new Object)
  }

  def foo(ns: AnyRef*): Unit = {
    System.out.println(42)
  }
}
