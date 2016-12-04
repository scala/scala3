import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 101, classesWithReachableMethods = 12, reachableMethods = 61)
  def main(args: Array[String]): Unit = {
    try {
      ???
    } catch {
      case _: NotImplementedError => System.out.println(42)
    }
  }
}
