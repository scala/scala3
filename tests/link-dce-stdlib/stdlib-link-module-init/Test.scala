import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 350, classesWithReachableMethods = 300, reachableMethods = 600)
  def main(args: Array[String]): Unit = {
    Predef // Just load Predef module
  }
}
