import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 6, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    System.out.println((() => 42)())
  }
}