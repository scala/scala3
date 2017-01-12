import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 19, classesWithReachableMethods = 6, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    lazy val x = 42
    System.out.println(x)
  }
}