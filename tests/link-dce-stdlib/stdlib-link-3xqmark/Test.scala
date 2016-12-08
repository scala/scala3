import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 294, classesWithReachableMethods = 248, reachableMethods = 472)
  def main(args: Array[String]): Unit = {
    try {
      ???
    } catch {
      case _: NotImplementedError => System.out.println(42)
    }
  }
}
