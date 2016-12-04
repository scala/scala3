import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 89, classesWithReachableMethods = 9, reachableMethods = 57)
  def main(args: Array[String]): Unit = {
    Predef // Just load Predef module
  }
}
