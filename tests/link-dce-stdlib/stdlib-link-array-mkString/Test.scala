import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 332, classesWithReachableMethods = 284, reachableMethods = 573)
  def main(args: Array[String]): Unit = {
    System.out.println(Array(1, 2, 3).mkString(", "))
  }
}
