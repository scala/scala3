import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 283, classesWithReachableMethods = 243, reachableMethods = 464)
  def main(args: Array[String]): Unit = {
    Predef // Just load Predef module
  }
}
