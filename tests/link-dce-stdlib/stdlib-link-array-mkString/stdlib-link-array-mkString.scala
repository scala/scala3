import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 132, classesWithReachableMethods = 15, reachableMethods = 74)
  def main(args: Array[String]): Unit = {
    System.out.println(Array(1, 2, 3).mkString(", "))
  }
}
