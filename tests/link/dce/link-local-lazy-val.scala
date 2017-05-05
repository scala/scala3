import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    lazy val x = 42
    System.out.println(x)
  }
}