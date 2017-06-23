import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    System.out.println((() => 42)())
  }
}