import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 6, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    System.out.println("hello")
    System.out.println(1)
    System.out.println(true)
  }
}
