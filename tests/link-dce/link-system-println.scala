import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 19, classesWithReachableMethods = 6, reachableMethods = 9)
  def main(args: Array[String]): Unit = {
    System.out.println("hello")
    System.out.println(1)
    System.out.println(true)
  }
}
