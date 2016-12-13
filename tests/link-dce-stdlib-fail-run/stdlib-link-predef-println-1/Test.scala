import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 150, classesWithReachableMethods = 100, reachableMethods = 150)
  def main(args: Array[String]): Unit = {
    println(42)
  }
}
