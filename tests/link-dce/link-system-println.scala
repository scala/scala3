import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 18, classesWithReachableMethods = 5, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    System.out.println("hello")
    System.out.println(1)
    System.out.println(true)
  }
}
