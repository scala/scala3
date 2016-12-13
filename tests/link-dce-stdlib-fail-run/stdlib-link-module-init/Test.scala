import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 300, classesWithReachableMethods = 250, reachableMethods = 500)
  def main(args: Array[String]): Unit = {
    Predef // Just load Predef module
  }
}
